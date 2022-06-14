from dataclasses import asdict, astuple, dataclass
from functools import singledispatch
from typing import TypeVar

import gym
from gym.spaces import MultiBinary, Discrete, MultiDiscrete, Box
import numpy as np


T = TypeVar("T")

@dataclass
class Obs:
    nodes: T
    edges: T
    permitted_actions: T
    cursor_position: T
    vars_in_scope: T
    assignment: T
    
    def to_space(self):
        return gym.spaces.Dict(**asdict(self))
    
    
class FlattenObservation(gym.ObservationWrapper):
    def __init__(self, env: gym.Env) -> None:
        super().__init__(env)
        spaces = Obs(**self.observation_space.spaces)
        self.orig_obs_space = spaces.to_space()
    
        def get_sizes(spaces):
            for space in astuple(spaces):
                if isinstance(space, Discrete):
                    yield 1
                elif isinstance(space, MultiDiscrete):
                    yield space.nvec.size
                elif isinstance(space, MultiBinary):
                    yield np.prod(space.shape)
                else:
                    raise NotImplementedError
                
        
        self.observation_space = Box(
            shape=[sum(get_sizes(spaces))],
            low=-np.inf,
            high=np.inf,
        )
        
    def observation(self, observation):
        obs = np.concatenate(
            astuple(Obs(**{k: np.array([v]) if isinstance(v, int) else v.flatten() for k, v in observation.items()}))
        )
        return obs
            