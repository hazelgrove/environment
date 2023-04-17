import os
import time
from collections import deque
from cProfile import run

import numpy as np
import torch
import yaml
from git import Repo
from gym.wrappers.time_limit import TimeLimit
from run_logger import RunLogger
from stable_baselines3.common.monitor import Monitor
from stable_baselines3.common.vec_env import DummyVecEnv, SubprocVecEnv

from agent import utils
from agent.arguments import get_args
from agent.envs import Env, PLEnv, VecPyTorch
from agent.policy import GNNPolicy, Policy, TestPolicy
from agent.ppo import PPO
from agent.storage import RolloutStorage
from envs.test_env import TestEnv
from evaluation import Evaluator, PLEvaluator
from logger import get_charts, get_metadata

from ray.air import session
from ray.air.integrations.wandb import setup_wandb

class Trainer:
    @staticmethod
    def get_policy(envs, params, device):
        base_kwargs = params["base"]
        policy = Policy(
            envs.observation_space, envs.action_space, base_kwargs=base_kwargs
        )
        return policy

    @staticmethod
    def get_env(params, device):
        envs = Env.make_vec_envs(
            params["env_name"],
            params["seed"],
            params["num_processes"],
            params["return"]["gamma"],
            device,
            False,
        )

        return envs

    @staticmethod
    def evaluate(
        actor_critic,
        logger,
        env_name,
        seed,
        num_processes,
        device,
        update,
        max_episode_steps,
        eval_kwargs,
    ):
        Evaluator.evaluate(
            actor_critic,
            env_name,
            seed,
            num_processes,
            device,
            max_episode_steps,
            eval_kwargs,
        )

    @staticmethod
    def update_curriculum(envs, reward):
        return

    @staticmethod
    def update_curriculum(envs, reward):
        return

    @classmethod
    def train(cls, logger, params, log_name, render, save_dir):
        # config = {"project": log_name}
        wandb = setup_wandb(project=log_name, group="assistant_rl", api_key_file="/RL_env/wandb_api_key")

        if log_name != "test":
            save_dir = os.path.join(save_dir, log_name)
            try:
                os.makedirs(save_dir)
            except OSError:
                pass
        else:
            save_dir = None

        # Only use one process if we are rendering
        if render:
            params["num_processes"] = 1

        params["base"]["num_assignments"] = params["env"]["num_assignments"]

        torch.manual_seed(params["seed"])
        torch.cuda.manual_seed_all(params["seed"])

        if (
            params["cuda"]
            and torch.cuda.is_available()
            and params["cuda_deterministic"]
        ):
            torch.backends.cudnn.benchmark = False
            torch.backends.cudnn.deterministic = True
        print(f"Cuda Availability: {torch.cuda.is_available()}")

        torch.set_num_threads(1)
        device = torch.device("cuda:0" if params["cuda"] else "cpu")

        envs = cls.get_env(params, device)

        actor_critic = cls.get_policy(envs, params, device)
        actor_critic.to(device)

        agent = PPO(
            actor_critic,
            **params["ppo"],
        )

        rollouts = RolloutStorage(
            params["num_steps"],
            params["num_processes"],
            envs.observation_space.shape,
            envs.action_space,
            actor_critic.recurrent_hidden_state_size,
        )

        obs = envs.reset()

        rollouts.obs[0].copy_(obs)
        rollouts.to(device)

        episode_rewards = deque(maxlen=1000)

        start = time.time()
        num_updates = (
            int(params["num_env_steps"])
            // params["num_steps"]
            // params["num_processes"]
        )

        last_eval_reward = 0.0
        for j in range(num_updates):
            if params["use_linear_lr_decay"]:
                # decrease learning rate linearly
                utils.update_linear_schedule(
                    agent.optimizer, j, num_updates, params["ppo"]["lr"]
                )

            for step in range(params["num_steps"]):
                # Sample actions
                with torch.no_grad():
                    (
                        value,
                        action,
                        action_log_prob,
                        recurrent_hidden_states,
                    ) = actor_critic.act(
                        rollouts.obs[step],
                        rollouts.recurrent_hidden_states[step],
                        rollouts.masks[step],
                    )

                if render:
                    print(f"Action: {action}")
                    breakpoint()
                obs, reward, done, infos = envs.step(action.reshape((-1,)))

                if render:
                    envs.render(mode="human")
                    print()

                    if done[0]:
                        print(f"Reward: {reward}")
                        print("---------------Environment reset---------------")

                for info in infos:
                    if "episode" in info.keys():
                        episode_rewards.append(info["episode"]["r"])

                # If done then clean the history of observations.
                masks = torch.FloatTensor([[0.0] if done_ else [1.0] for done_ in done])
                bad_masks = torch.FloatTensor(
                    [
                        [0.0] if "bad_transition" in info.keys() else [1.0]
                        for info in infos
                    ]
                )
                rollouts.insert(
                    obs,
                    recurrent_hidden_states,
                    action,
                    action_log_prob,
                    value,
                    reward,
                    masks,
                    bad_masks,
                )

            with torch.no_grad():
                next_value = actor_critic.get_value(
                    rollouts.obs[-1],
                    rollouts.recurrent_hidden_states[-1],
                    rollouts.masks[-1],
                ).detach()

            rollouts.compute_returns(
                next_value,
                **params["return"],
            )

            value_loss, action_loss, dist_entropy = agent.update(rollouts)

            rollouts.after_update()

            # save for every interval-th episode or for the last epoch
            if (
                j % params["save_interval"] == 0 or j == num_updates - 1
            ) and save_dir is not None:
                torch.save(
                    [
                        actor_critic.state_dict(),
                        getattr(utils.get_vec_normalize(envs), "obs_rms", None),
                    ],
                    os.path.join(save_dir, str(logger.run_id) + ".pt"),
                )

            mean_episode_reward = np.mean(episode_rewards)
            # cls.update_curriculum(envs, mean_episode_reward)
            metrics_train = {}
            if j % params["log_interval"] == 0 and len(episode_rewards) > 1:
                total_num_steps = (
                    (j + 1) * params["num_processes"] * params["num_steps"]
                )
                end = time.time()

                grad_norm = 0
                parameters = [
                    p
                    for p in actor_critic.parameters()
                    if p.grad is not None and p.requires_grad
                ]
                for p in parameters:
                    param_norm = p.grad.detach().data.norm(2)
                    grad_norm += param_norm.item() ** 2
                grad_norm = grad_norm**0.5

                fps = int(total_num_steps / (end - start))

                print(
                    "Updates {}, num timesteps {}, FPS {} \n Last {} training episodes: mean/median reward {:.2f}/{:.2f}, min/max reward {:.1f}/{:.1f}\n Gradient norm {:.3f}\n Policy loss {:.3E}, value loss {:.3E}, policy entropy {:.3E}\n".format(
                        j,
                        total_num_steps,
                        fps,
                        len(episode_rewards),
                        mean_episode_reward,
                        np.median(episode_rewards),
                        np.min(episode_rewards),
                        np.max(episode_rewards),
                        grad_norm,
                        action_loss,
                        value_loss,
                        dist_entropy,
                    )
                )
                metrics_train = {"train/reward": mean_episode_reward}

            metrics_eval = {}
            if (
                params["eval_interval"] > 0
                and len(episode_rewards) > 1
                and j % params["eval_interval"] == 0
            ):
                # obs_rms = utils.get_vec_normalize(envs).obs_rms
                eval_reward = cls.evaluate(
                    actor_critic,
                    # obs_rms,
                    params["env_name"],
                    params["seed"],
                    params["num_processes"],
                    device,
                    params["env"]["max_episode_steps"],
                    params["eval"],
                )
                last_eval_reward = eval_reward

                if logger is not None:
                    logger.log(
                        update=j,
                        mean_episode_rewards=mean_episode_reward,
                        eval_reward=eval_reward,
                        fps=fps,
                        episode_timesteps=total_num_steps,
                        gradient_norm=grad_norm,
                        policy_loss=action_loss,
                        value_loss=value_loss,
                        policy_entropy=dist_entropy,
                        run_id = str(logger.run_id),
                    )
                metrics_eval = {"eval/reward": eval_reward}   
            else:
                if logger is not None:
                    logger.log(
                        update=j,
                        mean_episode_rewards=mean_episode_reward,
                        eval_reward=last_eval_reward,
                        fps=fps,
                        episode_timesteps=total_num_steps,
                        gradient_norm=grad_norm,
                        policy_loss=action_loss,
                        value_loss=value_loss,
                        policy_entropy=dist_entropy,
                        run_id=str(logger.run_id),
                    )
            
            session.report({**metrics_train, **metrics_eval})
            wandb.log({**metrics_train, **metrics_eval})


class GNNTrainer(Trainer):
    @staticmethod
    def get_policy(envs, params, device):
        base_kwargs = params["base"]
        policy = GNNPolicy(
            envs.get_attr("orig_obs_space")[0],
            envs.get_attr("action_space")[0],
            envs.get_attr("num_actions")[0],
            base_kwargs=base_kwargs,
            device=device,
        )

        return policy

    @staticmethod
    def get_env(params, device):
        env_kwargs = params["env"]
        envs = PLEnv.make_vec_envs(
            params["seed"], params["num_processes"], device, **env_kwargs
        )

        return envs

    @staticmethod
    def evaluate(
        actor_critic,
        env_name,
        seed,
        num_processes,
        device,
        max_episode_steps,
        eval_kwargs,
    ):
        return PLEvaluator.evaluate(
            actor_critic,
            env_name,
            seed,
            num_processes,
            device,
            max_episode_steps,
            eval_kwargs,
        )

    @staticmethod
    def update_curriculum(envs, reward):
        envs.get_attr("update_curriculum")(reward)

    @staticmethod
    def update_curriculum(envs, reward):
        envs.get_attr("update_curriculum")(reward)


class TestTrainer(Trainer):
    @staticmethod
    def get_policy(envs, params, device):
        policy = TestPolicy(
            device=device,
        )

        return policy

    @staticmethod
    def make_env(seed, rank, max_episode_steps):
        def _thunk():
            # Arguments for env are fixed according to the implementation of the C code
            env = TestEnv(
                num_nodes=5,
            )
            env.seed(seed + rank)

            env = TimeLimit(env, max_episode_steps=max_episode_steps)
            env = Monitor(env)
            return env

        return _thunk

    @classmethod
    def make_vec_envs(
        cls,
        seed,
        num_processes,
        device,
    ):
        envs = [
            cls.make_env(
                seed,
                i,
                1,
            )
            for i in range(num_processes)
        ]

        if len(envs) > 1:
            envs = SubprocVecEnv(envs)
        else:
            envs = DummyVecEnv(envs)

        envs = VecPyTorch(envs, device)

        return envs

    @classmethod
    def get_env(cls, params, device):
        envs = cls.make_vec_envs(1, 8, device)

        return envs


if __name__ == "__main__":
    args = get_args()

    with open("params.yaml", "r") as file:
        params = yaml.safe_load(file)

    logger = RunLogger(os.getenv("GRAPHQL_ENDPOINT"))
    logger.create_run(
        metadata=get_metadata(Repo(".")),
        charts=get_charts(),
    )
    logger.update_metadata(
        {
            "parameters": params,
            "run_id": logger.run_id,
            "name": args.log_name,
        }
    )

    params["run_id"] = logger.run_id

    if args.gnn:
        GNNTrainer.train(
            logger=logger,
            params=params,
            log_name=args.log_name,
            render=args.render,
            save_dir=args.save_dir,
        )
    else:
        Trainer.train(args.log_name, render=args.render, save_dir=args.save_dir)
