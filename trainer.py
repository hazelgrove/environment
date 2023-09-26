import os
import time
from collections import deque
from cProfile import run
from typing import Optional
import random

import numpy as np
import torch
import yaml
from git import Repo
from gym.wrappers.time_limit import TimeLimit
from run_logger import RunLogger,get_load_params
from stable_baselines3.common.monitor import Monitor
from stable_baselines3.common.vec_env import DummyVecEnv, SubprocVecEnv

from agent import utils
from agent.arguments import get_args, read_params
from agent.envs import Env, PLEnv, VecPyTorch
from agent.policy import GNNPolicy, Policy, TestPolicy
from agent.ppo import PPO
from agent.storage import RolloutStorage
from envs.test_env import TestEnv
from evaluation import Evaluator, PLEvaluator
from logger import get_charts, get_metadata

from ray.air.integrations.wandb import setup_wandb
import wandb


class Trainer:
    def __init__(self, 
                 log_name: str,
                 params: dict,
                 sweep: bool = False,
                 save_dir: Optional[str] = None,): 
        self.log_name = log_name 
        self.params = params
        
        # Setup WandB Logger
        project_name = self.params['project_name'] if 'project_name' in self.params.keys() else 'assistant_rl'
        api_key_file = '/RL_env/wandb_api_key'
        if sweep:
            self.logger = setup_wandb(project=project_name, config=self.params, group=self.log_name, api_key_file=api_key_file)
        else:
            with open(api_key_file, "r") as f:
                api_key = f.readline()
            os.environ["WANDB_API_KEY"] = api_key
            
            wandb.login()
            self.logger = wandb.init(
                project=project_name, 
                config=self.params,
                notes=self.log_name,
            )
        
        # Setup directory for checkpoints
        if save_dir is not None:
            self.save_dir = os.path.join(save_dir, self.logger.name)
            try:
                os.makedirs(self.save_dir)
            except OSError:
                pass
        else:
            self.save_dir = None
        
        # Save hyperparameters to directory
        with open(os.path.join(self.save_dir, "params.yaml"),'w') as file :
            yaml.safe_dump(self.params, file)

    def get_policy(self,envs, device):
        base_kwargs = self.params["base"]
        policy = Policy(
            envs.observation_space, envs.action_space, base_kwargs=base_kwargs
        )
        return policy

    def get_env(self, device):
        envs = Env.make_vec_envs(
            self.params["env_name"],
            self.params["seed"],
            self.params["num_processes"],
            self.params["return"]["gamma"],
            device,
            False,
        )

        return envs

    @staticmethod
    def evaluate(
        self, 
        actor_critic,
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

    def train(self, render=False, save_dir=None, sweep=False):
        # Setup reproducibility
        seed = self.params['seed']
        torch.manual_seed(seed)
        np.random.seed(seed)
        random.seed(seed)

        if (
            self.params["cuda"]
            and torch.cuda.is_available()
        ):
            torch.cuda.manual_seed_all(seed)
            torch.backends.cudnn.benchmark = False
            torch.backends.cudnn.deterministic =  self.params["cuda_deterministic"]
            
        # Only use one process if we are rendering
        if render:
            self.params["num_processes"] = 1

        # Setup PyTorch
        torch.set_num_threads(1)
        device = torch.device("cuda:0" if self.params["cuda"] else "cpu")

        # Get environment
        envs = self.get_env(device)

        actor_critic = self.get_policy(envs, device)
        actor_critic.to(device)

        agent = PPO(
            actor_critic,
            **self.params["ppo"],
        )

        rollouts = RolloutStorage(
            self.params["num_steps"],
            self.params["num_processes"],
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
            int(self.params["num_env_steps"])
            // self.params["num_steps"]
            // self.params["num_processes"]
        )

        last_eval_reward = 0.0
        for j in range(num_updates):
            if self.params["use_linear_lr_decay"]:
                # decrease learning rate linearly
                log_lr = utils.update_linear_schedule(
                    agent.optimizer, j, num_updates, self.params["ppo"]["lr"],
                    targ_lr=self.params['ppo']['end_lr'] if 'end_lr' in self.params['ppo'] else None 
                )
                if 'entropy_coeff_decay' in self.params['ppo'] and self.params['ppo']['entropy_coeff_decay']:
                    start_entropy = self.params['ppo']['start_ent_coeff'] if 'start_ent_coeff' in self.params['ppo'] else None 
                    new_entropy = utils.update_entropy_schedule(j,num_updates,self.params['ppo']['entropy_coef'], initial_ent=start_entropy)
                    agent.set_entropy_coeff(new_entropy)

            for step in range(self.params["num_steps"]):
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
                try:
                    obs, reward, done, infos = envs.step(action.reshape((-1,)))
                except EOFError: 
                    print(action)
                    raise EOFError()

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
                **self.params["return"],
            )

            value_loss, action_loss, dist_entropy = agent.update(rollouts)

            rollouts.after_update()

            # save for every interval-th episode or for the last epoch
            if (
                j % self.params["save_interval"] == 0 or j == num_updates - 1
            ) and self.save_dir is not None:
                torch.save(
                    [
                        actor_critic.state_dict(),
                        getattr(utils.get_vec_normalize(envs), "obs_rms", None),
                    ],
                    os.path.join(self.save_dir, "params.pt"),
                )

            # Log train data
            if j % self.params["log_interval"] == 0 and len(episode_rewards) > 1:
                total_num_steps = (
                    (j + 1) * self.params["num_processes"] * self.params["num_steps"]
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
                
                mean_episode_reward = np.mean(episode_rewards)
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
                
                metrics_train = {
                    "train/reward": mean_episode_reward,
                    "train/total_num_steps": total_num_steps,
                    "train/fps": fps,
                    "train/grad_norm": grad_norm,
                    "train/action_loss": action_loss,
                    "train/value_loss": value_loss,
                    "train/dist_entroy": dist_entropy,
                }
                self.logger.log(metrics_train, step=j)

            # Log eval data
            if (
                self.params["eval_interval"] > 0
                and len(episode_rewards) > 1
                and j % self.params["eval_interval"] == 0
            ):
                eval_reward = self.evaluate(
                    actor_critic,
                    self.params["env_name"],
                    self.params["seed"],
                    self.params["num_processes"],
                    device,
                    self.params["env"]["max_episode_steps"],
                    self.params["eval"],
                )
                last_eval_reward = eval_reward

                metrics_eval = {
                    "eval/reward": eval_reward,
                }
                self.logger.log(metrics_eval, step=j)


class GNNTrainer(Trainer):
    def get_policy(self,envs, device):
        base_kwargs = self.params["base"]
        print(base_kwargs)
        policy = GNNPolicy(
            envs.get_attr("orig_obs_space")[0],
            envs.get_attr("action_space")[0],
            envs.get_attr("num_actions")[0],
            base_kwargs=base_kwargs,
            device=device,
            done_action=envs.get_attr('done_action')[0],
        )

        return policy

    def get_env(self, device):
        env_kwargs = self.params["env"]
        envs = PLEnv.make_vec_envs(
            self.params["seed"], self.params["num_processes"], device, **env_kwargs
        )

        return envs

    def evaluate(
        self,
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

    def update_curriculum(self,envs, reward):
        envs.get_attr("update_curriculum")(reward)

    def update_curriculum(self,envs, reward):
        envs.get_attr("update_curriculum")(reward)


class ResumeGNNTrainer(GNNTrainer):
    def __init__(self,log_name,params,resume_id,resume_name,runLogger):
        loaded_params = get_load_params(resume_id, runLogger)
        for field in params['resume_carryover']:
            params[field] = loaded_params[field]

        super().__init__(log_name,params,runLogger)

        self.resume_id = resume_id 
        self.resume_name = resume_name
        self.log_name = log_name 
        self.runLogger = runLogger
        self.params = params


    def get_policy(self,envs, device):

        base_kwargs = self.params["base"]
        policy = GNNPolicy(
            envs.get_attr("orig_obs_space")[0],
            envs.get_attr("action_space")[0],
            envs.get_attr("num_actions")[0],
            base_kwargs=base_kwargs,
            device=device,
            done_action=envs.get_attr('done_action')[0],
        )
        # load model 
        model_path = os.path.join("save", self.resume_name, str(self.resume_id) + ".pt")
        policy.load_state_dict(torch.load(model_path)[0])

        return policy
    
    #inherit the rest from GnnTrainer 


class TestTrainer(Trainer):
    def get_policy(self,envs, device):
        policy = TestPolicy(
            device=device,
        )

        return policy

    def make_env(self,seed, rank, max_episode_steps):
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

    def make_vec_envs(
        self,
        seed,
        num_processes,
        device,
    ):
        envs = [
            self.make_env(
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

    def get_env(self, device):
        envs = self.make_vec_envs(1, 8, device)
        return envs


if __name__ == "__main__":
    args = get_args()

    params = read_params(config_path)

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
        trainer = GNNTrainer(args.log_name,params,logger)
        trainer.train(
            render=args.render, save_dir=args.save_dir
        )
    else:
        trainer = Trainer()
        trainer.train(args.log_name, render=args.render, save_dir=args.save_dir)
