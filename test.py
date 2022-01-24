"""
Testing various functionalities of libraries
"""
import numpy as np
import gym
from stable_baselines3.common.env_checker import check_env
from stable_baselines3 import DQN, A2C
from stable_baselines3.dqn.policies import DQNPolicy, MlpPolicy
from stable_baselines3.common.vec_env import SubprocVecEnv


def test_multiproc(env_id, num_proc=4):
    def make_env():
        def _init():
            env = gym.make(env_id)
            return env

        return _init

    env = SubprocVecEnv([make_env() for _ in range(num_proc)])
    model = A2C("MlpPolicy", env, verbose=0)
    model.learn(total_timesteps=10)

    obs = env.reset()
    action, _states = model.predict(obs)
    obs, rewards, dones, info = env.step(action)
    print(rewards)


def main():
    test_multiproc(env_id="gym_basic:test-v0")


if __name__ == "__main__":
    main()
