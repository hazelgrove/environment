from envs.ast_env import ASTEnv
import numpy as np
from gym.wrappers.flatten_observation import FlattenObservation
from gym.spaces import unflatten, flatten


def main():
    env = ASTEnv(
        max_num_nodes=20,
        num_node_descriptor=50,
        num_assignments=1,
        code_per_assignment=[1],
        num_actions=80,
    )
    wrapped_env = FlattenObservation(env)
    
    print(env.observation_space.sample())
    

if __name__ == "__main__":
    main()