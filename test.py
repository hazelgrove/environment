import numpy as np
from gym.spaces import flatten, unflatten
import torch
from agent.envs import VecPyTorch
from agent.policy import GNNPolicy

from envs.ast_env import ASTEnv
from agent.base import GNNBase
from agent.arguments import get_args
from agent.wrapper import FlattenObservation
import ipdb


def main():
    ipdb.set_trace()
    args = get_args()
    device = torch.device("cpu")
    
    env = ASTEnv(
        max_num_nodes=20,
        num_node_descriptor=50,
        num_assignments=1,
        code_per_assignment=[1],
        num_actions=80,
    )
    obs = env.reset()
    print(obs)
    env = FlattenObservation(env)
    
    policy = GNNPolicy(
        env.orig_obs_space,
        env.action_space,
        base_kwargs={},
    )

    obs = env.reset()
    obs = np.array([obs, obs])
    obs = torch.from_numpy(obs).float()

    policy.act(obs, None, None)
    

if __name__ == "__main__":
    main()
