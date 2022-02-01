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


def test_multiproc(env_id, num_proc=2):
    def make_env():
        def _init():
            env = gym.make(env_id)
            return env

        return _init

    env = SubprocVecEnv([make_env() for _ in range(num_proc)])
    model = PPO(MultiInputActorCriticPolicy(env.observation_space, env.action_space, lr_schedule=(0.5, 0.4)), env, verbose=0)
    model.learn(total_timesteps=10)

    obs = env.reset()
    action, _states = model.predict(obs)
    obs, rewards, dones, info = env.step(action)
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
    testlib.print_struct(ctypes.byref(test))


def main():
    test_multiproc("gym_basic:ast-v0")


if __name__ == "__main__":
    main()
