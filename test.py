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
from agent.batch import collate
import ipdb
import gym


def main():
    env = ASTEnv(
                max_num_nodes=100,
                num_node_descriptor=33,
                num_assignments=1,
                code_per_assignment=[2],
                num_actions=58,
                assignment_dir="data/random_action",
            )
    obs_space = Obs(**env.observation_space.spaces)
    env = FlattenObservation(env)
    
    obs = env.reset()
    inputs = torch.tensor([obs, obs])
    
    inputs = Obs(
            *torch.split(
                inputs,
                [get_size(space) for space in astuple(obs_space)],
                dim=-1,
            )
        )
    base = GNNBase()
    base(asdict(inputs))
    

if __name__ == "__main__":
    main()
