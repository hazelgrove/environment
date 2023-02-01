import copy
import ctypes
import random
from typing import Any, List, Optional, Tuple, TypedDict, TypeVar, Union

import gym
import numpy as np
import numpy.typing as npt


class TestEnv(gym.Env):
    def __init__(
        self,
        num_nodes: int
    ):
        super(TestEnv, self).__init__()
        
        # Plus one to account for -1
        node_nvec = 2 * np.ones(num_nodes)
        
        self.num_nodes = num_nodes
        
        self.action_space = gym.spaces.Discrete(2)
        self.observation_space = gym.spaces.MultiDiscrete(node_nvec)
        
        self.state = None

    def step(self, action: int):
        reward = 0
        if self.state[0] == self.state[-1] and action == 0:
            reward = 1
        elif self.state[0] != self.state[-1] and action == 1:
            reward = 1
        done = True

        return self.state, reward, done, {}

    def reset(self):
        self.state = np.random.randint(0, 2, self.num_nodes)
        
        return self.state

    def render(self, mode=None) -> None:
        print(f"Current state: {self.state}")
