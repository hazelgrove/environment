from dataclasses import asdict, astuple
import numpy as np
from gym.spaces import flatten, unflatten
from stable_baselines3.common.vec_env import DummyVecEnv
import torch
from agent.envs import VecPyTorch, make_env
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
    obs = env.reset()
    obs = torch.tensor([obs, obs, obs])
    
    inputs = Obs(
            *torch.split(
                obs,
                [get_size(space) for space in astuple(obs_space)],
                dim=-1,
            )
        )
    
    gnn = GNNBase()
    value, actor_features = gnn(asdict(inputs))
    
    qkv = QKV(num_actions=64, embedding_size=32)
    attn = qkv(actor_features)
    
    print(asdict(inputs)["permitted_actions"])
    

if __name__ == "__main__":
    main()
