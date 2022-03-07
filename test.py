"""
Testing various functionalities of libraries
"""
import numpy as np
import gym
from stable_baselines3.common.env_checker import check_env
from stable_baselines3 import DQN, A2C, PPO
from stable_baselines3.dqn.policies import DQNPolicy, MlpPolicy
from stable_baselines3.common.vec_env import SubprocVecEnv, DummyVecEnv
from stable_baselines3.common.policies import MultiInputActorCriticPolicy
import ctypes


def test_env(env_id):
    env = gym.make(env_id)
    
    for i in range(5):
        print("-----------------------")
        print(f"Timestep: {i + 1}")
        
        env.reset()
        print("Reset environment.\n")
        
        done = False
        while not done:
            action = env.action_space.sample()
            print(f"Action taken: {action}")

            obs, rewards, done, info = env.step(action)
            
            env.render()
            
            print(f"Reward: {rewards}\n")
        print("Done!")
    env.close()


def test_multiproc(env_id, num_proc=2):
    def make_env():
        def _init():
            env = gym.make(env_id)
            return env

        return _init

    env = SubprocVecEnv([make_env() for _ in range(num_proc)])

    obs = env.reset()
    
    action = [env.action_space.sample() for _ in range(num_proc)]
    print(action)

    obs, rewards, done, info = env.step(action)
    print(obs)
    print(rewards)

def test_ctype(arr):
    testlib = ctypes.CDLL('clib/test.so')
    
    arr_type = ctypes.c_int * len(arr)
    testlib.plus_one.restype = None
    testlib.plus_one(ctypes.c_int(len(arr)), arr_type(*arr))
    return arr


def test_struct():
    testlib = ctypes.CDLL('clib/test.so')
    class abc(ctypes.Structure):
        _fields_ = [("a", (ctypes.c_int * 3) * 2),
                    ("b", ctypes.c_double),
                    ("c", ctypes.c_char)]
    
    test = abc()
    for i in range(2):
        for j in range(3):
            test.a[i][j] = 3 * i + j;
    test.b = 1.2
    test.c = b"c"
    testlib.change_struct(ctypes.byref(test))
    print(test.b)


def main():
    # test_struct()
    test_env("gym_basic:ast-v0")


if __name__ == "__main__":
    main()
