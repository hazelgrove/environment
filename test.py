import multiprocessing as mp
import gym
from stable_baselines3.common.vec_env import SubprocVecEnv
import numpy as np


def f():
    return gym.make("gym_basic:test-v0")


def main():
    n_envs = 2
    vec_fns = [f for _ in range(n_envs)]
    vec_envs = SubprocVecEnv(vec_fns)
    print("Observation 1:")
    print(vec_envs.reset())
    action = np.array([vec_envs.action_space.sample() for _ in range(n_envs)])
    print("Observation 2:")
    print(vec_envs.step(action))
