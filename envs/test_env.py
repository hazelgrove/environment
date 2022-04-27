import ctypes

import gym


class TestEnv(gym.Env):
    def __init__(self):
        super(TestEnv, self).__init__()

        # Define action space
        self.action_space = gym.spaces.Discrete(5)

        # Define States
        self.observation_space = gym.spaces.Dict(
            {"1": gym.spaces.Discrete(2), "2": gym.spaces.Discrete(2)}
        )

    def step(self, action):
        testlib = testlib = ctypes.CDLL("clib/test.so")

        state = {"1": 1, "2": 1}
        reward = testlib.get_reward(ctypes.c_int(action))
        done = True
        info = {}

        return state, reward, done, info

    def reset(self):
        state = {"1": 0, "2": 0}
        return state
