from functools import singledispatch

from gym.spaces import MultiBinary, Discrete
import numpy as np

@singledispatch
def flatten(space, x):
    raise NotImplementedError

@flatten.register(MultiBinary)
def _flatten_multiBinary(space, x):
    return np.asarray(x, dtype=space.dtype).flatten()

@flatten.register(Discrete)
def _flatten_discrete(space, x):
    pass