import numpy as np
from gym.spaces import flatten, unflatten
from gym.wrappers.flatten_observation import FlattenObservation
import torch
from agent.policy import GNNPolicy

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
    
    obs["vars_in_scope"][0] = 1
    flatten_obs = flatten(env.observation_space, obs)
    flatten_obs = np.vstack((flatten_obs, flatten_obs))
    policy = GNNPolicy(env=env)
    
    unflatten_obs = policy.unflatten_input(flatten_obs)
    print(obs)
    print(unflatten_obs)
    
    


if __name__ == "__main__":
    main()
