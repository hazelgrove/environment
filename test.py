from dataclasses import asdict, astuple
import numpy as np
from gym.spaces import flatten, unflatten
from stable_baselines3.common.vec_env import DummyVecEnv
import torch
from agent.envs import VecPyTorch
from agent.policy import GNNPolicy, get_size

from envs.ast_env import ASTEnv
from agent.base import GNNBase, TestBase
from agent.arguments import get_args
from agent.wrapper import FlattenObservation, Obs
from agent.distributions import QKV
from agent.batch import collate
from agent.arguments import read_params
import ipdb
import gym
import yaml


from trainer import TestTrainer


def main():
    params = read_params('params.yaml')

    testTrainer = TestTrainer()
    testTrainer.train(
        logger=None,
        params=params,
        log_name="test",
        render=False,
        save_dir=None,
    )
    

if __name__ == "__main__":
    main()
