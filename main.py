import os
import time
from collections import deque
from cProfile import run
from gym import Env

import ipdb
import numpy as np
import torch

from agent import utils
from agent.arguments import get_args
from agent.envs import PLEnv, Env
from agent.policy import GNNPolicy, Policy
from agent.ppo import PPO
from agent.storage import RolloutStorage
from evaluation import Evaluator, PLEvaluator
from logger import get_logger


class Trainer:
    @staticmethod
    def get_policy(envs, args):
        return Policy(
            envs.observation_space,
            envs.action_space,
            base_kwargs={'recurrent': args.recurrent_policy}
        )

    @staticmethod
    def setup_log():
        _, logger = get_logger()
        print(f"Logger: {logger}")
        return logger
    
    @staticmethod
    def get_env(args, device):
        return Env.make_vec_envs(
            args.env_name,
            args.seed,
            args.num_processes,
            args.gamma,
            args.log_dir,
            device,
            False,
        )
        
    @staticmethod
    def evaluate(
        actor_critic,
        obs_rms,
        env_name,
        seed,
        num_processes,
        eval_log_dir,
        device,
        max_episode_steps,
    ):
        Evaluator.evaluate(actor_critic,
                    obs_rms,
                    env_name,
                    seed,
                    num_processes,
                    eval_log_dir,
                    device,
                    max_episode_steps)
        
    @staticmethod
    def reset_env(env, num_processes):
        return env.reset()
    
    @classmethod
    def main(cls):
        args = get_args()
        
        if args.log:
            logger = cls.setup_log()
        else:
            logger = None

        torch.manual_seed(args.seed)
        torch.cuda.manual_seed_all(args.seed)

        if args.cuda and torch.cuda.is_available() and args.cuda_deterministic:
            torch.backends.cudnn.benchmark = False
            torch.backends.cudnn.deterministic = True
        print(f"Cuda: {torch.cuda.is_available()}")

        log_dir = os.path.expanduser(args.log_dir)
        eval_log_dir = log_dir + "_eval"
        utils.cleanup_log_dir(log_dir)
        utils.cleanup_log_dir(eval_log_dir)

        torch.set_num_threads(1)
        device = torch.device("cuda:0" if args.cuda else "cpu")

        envs = cls.get_env(args, device)

        actor_critic = cls.get_policy(envs, args)
        actor_critic.to(device)

        agent = PPO(
            actor_critic,
            args.clip_param,
            args.ppo_epoch,
            args.num_mini_batch,
            args.value_loss_coef,
            args.entropy_coef,
            lr=args.lr,
            eps=args.eps,
            max_grad_norm=args.max_grad_norm,
        )

        rollouts = RolloutStorage(
            args.num_steps,
            args.num_processes,
            envs.observation_space.shape,
            envs.action_space,
            actor_critic.recurrent_hidden_state_size,
        )

        obs = cls.reset_env(envs, args.num_processes)
        rollouts.obs[0].copy_(obs)
        rollouts.to(device)

        episode_rewards = deque(maxlen=10)

        start = time.time()
        num_updates = int(args.num_env_steps) // args.num_steps // args.num_processes
        for j in range(num_updates):

            if args.use_linear_lr_decay:
                # decrease learning rate linearly
                utils.update_linear_schedule(agent.optimizer, j, num_updates, args.lr)

            for step in range(args.num_steps):
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

                # print(f"Action: {action}")
                obs, reward, done, infos = envs.step(action)
                
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
                args.use_gae,
                args.gamma,
                args.gae_lambda,
                args.use_proper_time_limits,
            )

            value_loss, action_loss, dist_entropy = agent.update(rollouts)

            rollouts.after_update()

            # save for every interval-th episode or for the last epoch
            if (
                j % args.save_interval == 0 or j == num_updates - 1
            ) and args.save_dir != "":
                save_path = args.save_dir
                try:
                    os.makedirs(save_path)
                except OSError:
                    pass

                torch.save(
                    [
                        actor_critic,
                        getattr(utils.get_vec_normalize(envs), "obs_rms", None),
                    ],
                    os.path.join(save_path, args.env_name + ".pt"),
                )
            
            if j % args.log_interval == 0 and len(episode_rewards) > 1:
                total_num_steps = (j + 1) * args.num_processes * args.num_steps
                end = time.time()
                print(
                    "Updates {}, num timesteps {}, FPS {} \n Last {} training episodes: mean/median reward {:.1f}/{:.1f}, min/max reward {:.1f}/{:.1f}\n".format(
                        j,
                        total_num_steps,
                        int(total_num_steps / (end - start)),
                        len(episode_rewards),
                        np.mean(episode_rewards),
                        np.median(episode_rewards),
                        np.min(episode_rewards),
                        np.max(episode_rewards),
                    )
                )

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
                print(grad_norm)

                if logger is not None:
                    logger.log(
                        update=j,
                        mean_episode_rewards=np.mean(episode_rewards),
                        episode_timesteps=total_num_steps,
                        gradient_norms=grad_norm,
                        policy_loss=action_loss,
                        value_loss=value_loss,
                        policy_entropy=dist_entropy,
                    )

            if (
                args.eval_interval is not None
                and len(episode_rewards) > 1
                and j % args.eval_interval == 0
            ):
                obs_rms = utils.get_vec_normalize(envs).obs_rms
                cls.evaluate(
                    actor_critic,
                    obs_rms,
                    args.env_name,
                    args.seed,
                    args.num_processes,
                    eval_log_dir,
                    device,
                    args.max_episode_steps,
                )


class GNNTrainer(Trainer):
    def get_policy(envs, args):
        return GNNPolicy(
            envs.get_attr("orig_obs_space")[0],
            envs.get_attr("action_space")[0],
            envs.get_attr("num_actions")[0],
            base_kwargs={},
        )
    
    def get_env(args, device):
        return PLEnv.make_vec_envs(
            args.seed,
            args.num_processes,
            device,
            args.max_episode_steps,
        )
        
    @staticmethod
    def evaluate(
        actor_critic,
        obs_rms,
        env_name,
        seed,
        num_processes,
        eval_log_dir,
        device,
        max_episode_steps,
    ):
        PLEvaluator.evaluate(actor_critic,
                    obs_rms,
                    env_name,
                    seed,
                    num_processes,
                    eval_log_dir,
                    device,
                    max_episode_steps)
        
    @staticmethod
    def reset_env(env, num_processes):
        # Manually shift cursor
        env.reset()
        env.step(torch.tensor([
            [1] for _ in range(num_processes)
        ]))
        env.step(torch.tensor([
            [2] for _ in range(num_processes)
        ]))
        obs, _, _, _ = env.step(torch.tensor([
            [2] for _ in range(num_processes)
        ]))
        return obs
        

if __name__ == "__main__":
    args = get_args()
    if args.env_name == "pl":
        GNNTrainer.main()
    else:
        Trainer.main()
