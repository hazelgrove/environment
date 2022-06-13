import numpy as np
from gym.spaces import flatten, unflatten
from gym.wrappers.flatten_observation import FlattenObservation
import torch
from agent.envs import VecPyTorch
from agent.policy import GNNPolicy

from envs.ast_env import ASTEnv
from agent.base import GNNBase
from agent.arguments import get_args
from agent import algo
import ipdb


def main():
    ipdb.set_trace()
    args = get_args()
    device = torch.device("cpu")
    
    env = ASTEnv(
                max_num_nodes=100,
                num_node_descriptor=50,
                num_assignments=1,
                code_per_assignment=[1],
                num_actions=71,
            )
    obs_space = env.observation_space
    action_space = env.action_space
    env = FlattenObservation(env)
    
    policy = GNNPolicy(
        obs_space,
        action_space,
        base_kwargs={},
    )

    obs = env.reset()
    breakpoint()
    obs = np.array([obs, obs])
    obs = torch.from_numpy(obs).float()

    policy.act(obs, None, None)
    

if __name__ == "__main__":
    main()
