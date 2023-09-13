from collections import deque
import numpy as np
import torch

from agent import utils
from agent.envs import Env, PLEnv


class Evaluator:
    @staticmethod
    def get_envs(env_name, seed, num_processes, device, max_episode_steps, **kwargs):
        return Env.make_vec_envs(
            env_name,
            seed + num_processes,
            num_processes,
            None,
            device,
            True,
        )

    @classmethod
    def evaluate(
        cls,
        actor_critic,
        env_name,
        seed,
        num_processes,
        device,
        max_episode_steps,
        eval_kwargs,
    ):
        eval_envs = cls.get_envs(
            env_name,
            seed,
            num_processes,
            device,
            **eval_kwargs,
        )

        # vec_norm = utils.get_vec_normalize(eval_envs)
        # if vec_norm is not None:
        #     vec_norm.eval()
        #     vec_norm.obs_rms = obs_rms

        eval_episode_rewards = deque(maxlen=100)

        obs = eval_envs.reset()
        eval_recurrent_hidden_states = torch.zeros(
            num_processes, actor_critic.recurrent_hidden_state_size, device=device
        )
        eval_masks = torch.zeros(num_processes, 1, device=device)

        while len(eval_episode_rewards) < 100:
            with torch.no_grad():
                _, action, _, eval_recurrent_hidden_states = actor_critic.act(
                    obs,
                    eval_recurrent_hidden_states,
                    eval_masks,
                    deterministic=True,
                )

            # Obser reward and next obs
            obs, _, done, infos = eval_envs.step(action.reshape((-1,)))

            eval_masks = torch.tensor(
                [[0.0] if done_ else [1.0] for done_ in done],
                dtype=torch.float32,
                device=device,
            )

            for info in infos:
                if "episode" in info.keys():
                    eval_episode_rewards.append(info["episode"]["r"])

        eval_envs.close()

        mean_episode_reward = np.mean(eval_episode_rewards)
        
        print(
            " Evaluation using {} episodes: mean reward {:.5f}\n".format(
                len(eval_episode_rewards), mean_episode_reward
            )
        )
        
        return mean_episode_reward


class PLEvaluator(Evaluator):
    @staticmethod
    def get_envs(
        env_name, 
        seed, 
        num_processes, 
        device, 
        **kwargs,):
        return PLEnv.make_vec_envs(
            seed + num_processes,
            num_processes,
            device,
            **kwargs
        )


