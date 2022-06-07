import numpy as np
from gym.spaces import flatten, unflatten
from gym.wrappers.flatten_observation import FlattenObservation
import torch

from envs.ast_env import ASTEnv
from agent.base import GNNBase
import ipdb


def main():
    ipdb.set_trace()
    env = ASTEnv(
        max_num_nodes=20,
        num_node_descriptor=50,
        num_assignments=1,
        code_per_assignment=[1],
        num_actions=80,
    )
    obs = env.reset()
    
    obs = env.unpad_states(obs)
    
    x = obs["nodes"]
    edge_index = obs["edges"][:, :2].reshape((2, -1))
    edge_attr = obs["edges"][:, 2]
    assignment = obs["assignment"]
    cursor = obs["cursor_position"]
    
    x = torch.tensor([x, x])
    edge_index = torch.tensor([edge_index, edge_index])
    edge_attr = torch.tensor([edge_attr, edge_attr])
    assignment = torch.tensor([assignment, assignment])
    cursor = torch.tensor([cursor, cursor])
    
    base = GNNBase()
    base(x, edge_index, edge_attr, assignment, cursor)


if __name__ == "__main__":
    main()
