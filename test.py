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
    action_space = gym.spaces.Discrete(10)
    observation_space = gym.spaces.Discrete(10)
    
    
    actor_critic = GNNPolicy(observation_space, action_space, num_fixed_actions=10)
    
    torch.save(actor_critic, "model.pt")
    torch.load("model.pt")
    

if __name__ == "__main__":
    main()
