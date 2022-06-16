import numpy as np
from gym.spaces import flatten, unflatten
from stable_baselines3.common.vec_env import DummyVecEnv
import torch
from agent.envs import VecPyTorch, make_env
from agent.policy import GNNPolicy

from envs.ast_env import ASTEnv
from agent.base import GNNBase
from agent.arguments import get_args
from agent.wrapper import FlattenObservation
import ipdb


def main():
    args = get_args()
    
    env = ASTEnv(
        max_num_nodes=20,
        num_node_descriptor=50,
        num_assignments=1,
        code_per_assignment=[1],
        num_actions=71,
    )
    obs = env.reset()
    env.step(56)
    

if __name__ == "__main__":
    main()
