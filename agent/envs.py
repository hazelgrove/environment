import os

import gym
import numpy as np
import torch
from gym.spaces.box import Box
from gym.wrappers.clip_action import ClipAction
from gym.wrappers.time_limit import TimeLimit
from gym.utils.seeding import np_random
from stable_baselines3.common.atari_wrappers import (
    ClipRewardEnv,
    EpisodicLifeEnv,
    FireResetEnv,
    MaxAndSkipEnv,
    NoopResetEnv,
    WarpFrame,
)
from stable_baselines3.common.monitor import Monitor
from stable_baselines3.common.vec_env import DummyVecEnv, SubprocVecEnv, VecEnvWrapper
from stable_baselines3.common.vec_env.vec_normalize import VecNormalize as VecNormalize_

from agent.wrapper import FlattenObservation
from envs.ast_env import ASTEnv

try:
    import dmc2gym
except ImportError:
    pass

try:
    import roboschool
except ImportError:
    pass

try:
    import pybullet_envs
except ImportError:
    pass


class Env:
    @staticmethod
    def make_env(env_id, seed, rank, allow_early_resets):
        def _thunk():
            if env_id.startswith("dm"):
                _, domain, task = env_id.split(".")
                env = dmc2gym.make(domain_name=domain, task_name=task,seed=seed)
                env = ClipAction(env)
            else:
                env = gym.make(env_id,seed=seed)

            is_atari = hasattr(gym.envs, "atari") and isinstance(
                env.unwrapped, gym.envs.atari.AtariEnv
            )
            if is_atari:
                env = NoopResetEnv(env, noop_max=30)
                env = MaxAndSkipEnv(env, skip=4)

            if str(env.__class__.__name__).find("TimeLimit") >= 0:
                env = TimeLimitMask(env)

            env = Monitor(env, allow_early_resets=allow_early_resets)

            if is_atari:
                if len(env.observation_space.shape) == 3:
                    env = EpisodicLifeEnv(env)
                    if "FIRE" in env.unwrapped.get_action_meanings():
                        env = FireResetEnv(env)
                    env = WarpFrame(env, width=84, height=84)
                    env = ClipRewardEnv(env)
            elif len(env.observation_space.shape) == 3:
                raise NotImplementedError(
                    "CNN models work only for atari,\n"
                    "please use a custom wrapper for a custom pixel input env.\n"
                    "See wrap_deepmind for an example."
                )

            # If the input has shape (W,H,3), wrap for PyTorch convolutions
            obs_shape = env.observation_space.shape
            if len(obs_shape) == 3 and obs_shape[2] in [1, 3]:
                env = TransposeImage(env, op=[2, 0, 1])

            return env

        return _thunk

    @staticmethod
    def make_vec_envs(
        env_name,
        seed,
        num_processes,
        gamma,
        device,
        allow_early_resets,
        num_frame_stack=None,
    ):
        envs = [
            Env.make_env(env_name, seed, i, allow_early_resets)
            for i in range(num_processes)
        ]

        if len(envs) > 1:
            envs = SubprocVecEnv(envs)
        else:
            envs = DummyVecEnv(envs)

        if len(envs.observation_space.shape) == 1:
            if gamma is None:
                envs = VecNormalize(envs, norm_reward=False)
            else:
                envs = VecNormalize(envs, gamma=gamma)

        envs = VecPyTorch(envs, device)

        if num_frame_stack is not None:
            envs = VecPyTorchFrameStack(envs, num_frame_stack, device)
        elif len(envs.observation_space.shape) == 3:
            envs = VecPyTorchFrameStack(envs, 4, device)

        return envs


class PLEnv(Env):
    @staticmethod
    def make_env(
        seed,
        rank,
        max_episode_steps,
        perturbation,
        assignment_dir,
        cursor_start_pos,
        num_assignments,
        code_per_assignment,
        done_action, 
        ds_ratio = None,
        multi_ds=False,
        max_episode_steps_per_ds=None
    ):
        def _thunk():
            # Arguments for env are fixed according to the implementation of the C code
            env = ASTEnv(
                max_num_nodes=150,
                num_node_descriptor=107,
                num_assignments=num_assignments,
                code_per_assignment=code_per_assignment,
                num_actions=132,
                perturbation=perturbation,
                seed=seed,
                assignment_dir=assignment_dir,
                cursor_start_pos=cursor_start_pos,
                done_action=done_action,
                ds_ratio=ds_ratio,
                multi_ds=multi_ds,
                max_episode_steps_per_ds=max_episode_steps_per_ds,
            )
            setattr(env,'render_mode',False)
            env = FlattenObservation(env)
            
            # if render:
            #     env = RenderWrapper(env, mode=render_mode)
            env = TimeLimit(env, max_episode_steps=max_episode_steps)
            env = Monitor(env)
            return env

        return _thunk

    @staticmethod
    def make_vec_envs(
        seed,
        num_processes,
        device,
        max_episode_steps,
        perturbation,
        done_action,
        test_params,
        cursor_start_pos=None,
        assignment_dir =None, 
        num_assignments=None,
        code_per_assignment=None,
        ds_ratio=None,
        render=False,
    ):
        if render and num_processes > 1:
            raise ValueError("Rendering is not supported for multiple processes")
        
        #handle multi-ds code 
        if type(test_params) is not  list:
            ds_ratio = [1.0]
            test_params = [test_params]

        if ds_ratio is None or len(ds_ratio) != len(test_params):
            print('\nDS ratio either nonexistent or not the right length.')
            print('A uniform DS ratio will be assumed\n')
            ds_ratio = [1/float(len(test_params))] * len(test_params)
        for test_param in test_params: 
            print(test_param)
        code_per_assignment = [param['code_per_assignment']for param in test_params]
        num_assignments = [param['num_assignments'] for param in test_params]
        assignment_dir=[param['assignment_dir'] for param in test_params]
        cursor_start_pos = [param['cursor_start_pos'] for param in test_params]
        max_episode_steps_per_ds = [param['max_episode_steps'] for param in test_params]
        multi_DS =True
        # test that everything is input correctly 
        assert(type(code_per_assignment) is list )
        assert(type(num_assignments) in (list,int) )
        assert(type(assignment_dir) in (list,str) )
        assert(num_assignments is not None)
        assert(type(num_assignments) in (list, int)) 
        envs = [
            PLEnv.make_env(
                seed * 100 + i, # multiply by 100 because i don't want overlap b/t 1,2,3,4 seeded starts
                i,
                max_episode_steps,
                perturbation,
                assignment_dir,
                cursor_start_pos,
                num_assignments,
                code_per_assignment,
                done_action, 
                ds_ratio=ds_ratio,
                multi_ds=multi_DS,
                max_episode_steps_per_ds = max_episode_steps_per_ds
            )
            for i in range(num_processes)
        ]

        if len(envs) > 1:
            envs = SubprocVecEnv(envs)
        else:
            envs = DummyVecEnv(envs)

        envs = VecPyTorch(envs, device)

        return envs


# Checks whether done was caused my timit limits or not
class TimeLimitMask(gym.Wrapper):
    def step(self, action):
        obs, rew, done, info = self.env.step(action)
        if done and self.env._max_episode_steps == self.env._elapsed_steps:
            info["bad_transition"] = True

        return obs, rew, done, info

    def reset(self, **kwargs):
        return self.env.reset(**kwargs)


# Can be used to test recurrent policies for Reacher-v2
class MaskGoal(gym.ObservationWrapper):
    def observation(self, observation):
        if self.env._elapsed_steps > 0:
            observation[-2:] = 0
        return observation


class TransposeObs(gym.ObservationWrapper):
    def __init__(self, env=None):
        """
        Transpose observation space (base class)
        """
        super(TransposeObs, self).__init__(env)


class TransposeImage(TransposeObs):
    def __init__(self, env=None, op=[2, 0, 1]):
        """
        Transpose observation space for images
        """
        super(TransposeImage, self).__init__(env)
        assert len(op) == 3, "Error: Operation, " + str(op) + ", must be dim3"
        self.op = op
        obs_shape = self.observation_space.shape
        self.observation_space = Box(
            self.observation_space.low[0, 0, 0],
            self.observation_space.high[0, 0, 0],
            [obs_shape[self.op[0]], obs_shape[self.op[1]], obs_shape[self.op[2]]],
            dtype=self.observation_space.dtype,
        )

    def observation(self, ob):
        return ob.transpose(self.op[0], self.op[1], self.op[2])


class VecPyTorch(VecEnvWrapper):
    def __init__(self, venv, device):
        """Return only every `skip`-th frame"""
        super(VecPyTorch, self).__init__(venv)
        self.device = device
        # TODO: Fix data types

    def reset(self):
        obs = self.venv.reset()
        obs = torch.from_numpy(obs).float().to(self.device)
        return obs

    def step_async(self, actions):
        if isinstance(actions, torch.LongTensor):
            # Squeeze the dimension for discrete actions
            actions = actions.squeeze(1)
        actions = actions.detach().cpu().numpy()
        self.venv.step_async(actions)

    def step_wait(self):
        try: 
            obs, reward, done, info = self.venv.step_wait()
        except EOFError:
            print(self)
            raise EOFError()
        obs = torch.from_numpy(obs).float().to(self.device)
        reward = torch.from_numpy(reward).unsqueeze(dim=1).float()
        return obs, reward, done, info


class VecNormalize(VecNormalize_):
    def __init__(self, *args, **kwargs):
        super(VecNormalize, self).__init__(*args, **kwargs)
        self.training = True

    def _obfilt(self, obs, update=True):
        if self.obs_rms:
            if self.training and update:
                self.obs_rms.update(obs)
            obs = np.clip(
                (obs - self.obs_rms.mean) / np.sqrt(self.obs_rms.var + self.epsilon),
                -self.clip_obs,
                self.clip_obs,
            )
            return obs
        else:
            return obs

    def train(self):
        self.training = True

    def eval(self):
        self.training = False


# Derived from
# https://github.com/openai/baselines/blob/master/baselines/common/vec_env/vec_frame_stack.py
class VecPyTorchFrameStack(VecEnvWrapper):
    def __init__(self, venv, nstack, device=None):
        self.venv = venv
        self.nstack = nstack

        wos = venv.observation_space  # wrapped ob space
        self.shape_dim0 = wos.shape[0]

        low = np.repeat(wos.low, self.nstack, axis=0)
        high = np.repeat(wos.high, self.nstack, axis=0)

        if device is None:
            device = torch.device("cpu")
        self.stacked_obs = torch.zeros((venv.num_envs,) + low.shape).to(device)

        observation_space = gym.spaces.Box(
            low=low, high=high, dtype=venv.observation_space.dtype
        )
        VecEnvWrapper.__init__(self, venv, observation_space=observation_space)

    def step_wait(self):
        obs, rews, news, infos = self.venv.step_wait()
        self.stacked_obs[:, : -self.shape_dim0] = self.stacked_obs[
            :, self.shape_dim0 :
        ].clone()
        for (i, new) in enumerate(news):
            if new:
                self.stacked_obs[i] = 0
        self.stacked_obs[:, -self.shape_dim0 :] = obs
        return self.stacked_obs, rews, news, infos

    def reset(self):
        obs = self.venv.reset()
        if torch.backends.cudnn.deterministic:
            self.stacked_obs = torch.zeros(self.stacked_obs.shape)
        else:
            self.stacked_obs.zero_()
        self.stacked_obs[:, -self.shape_dim0 :] = obs
        return self.stacked_obs

    def close(self):
        self.venv.close()
