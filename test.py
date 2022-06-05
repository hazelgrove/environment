import numpy as np
from gym.spaces import flatten, unflatten
from gym.wrappers.flatten_observation import FlattenObservation

from envs.ast_env import ASTEnv


def main():
    env = ASTEnv(
        max_num_nodes=20,
        num_node_descriptor=50,
        num_assignments=1,
        code_per_assignment=[1],
        num_actions=80,
    )
    wrapped_env = FlattenObservation(env)

    obs = wrapped_env.reset()
    print(obs)


if __name__ == "__main__":
    main()
