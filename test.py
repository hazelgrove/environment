from dataclasses import asdict, astuple
import numpy as np
from gym.spaces import flatten, unflatten
from stable_baselines3.common.vec_env import DummyVecEnv
import torch
from agent.envs import VecPyTorch
from agent.policy import GNNPolicy, get_size

from envs.ast_env import ASTEnv
from agent.base import GNNBase
from agent.arguments import get_args
from agent.wrapper import FlattenObservation, Obs
from agent.distributions import QKV
import ipdb


def main():
    args = get_args()
    
    env = ASTEnv(
                max_num_nodes=50,
                num_node_descriptor=50,
                num_assignments=1,
                code_per_assignment=[1],
                num_actions=54,
            )
    obs_space = Obs(**env.observation_space.spaces)
    env = FlattenObservation(env)
    
    env.reset()
    breakpoint()
    env.step(53)
    env.step(0)
    env.step(2)
    _, _, done, _ = env.step(54)
    print(done)
    

if __name__ == "__main__":
    main()
