import gym
import numpy as np 

class ASTEnv(gym.Env):
    def __init__(self, num_actions, max_num_nodes):
        super(ASTEnv, self).__init__()

        self.action_space = gym.spaces.Discrete(num_actions)
        
        # Set observation space
        num_node_descriptor = 10 # TODO: Specify this number
        node_nvec = num_node_descriptor * np.ones(max_num_nodes)
        edge_nvec = max_num_nodes * np.ones((max_num_nodes ** 2, 2))
        self.observation_space = gym.spaces.Dict({
            'nodes': gym.spaces.MultiDiscrete(node_nvec),
            'edges': gym.spaces.MultiDiscrete(edge_nvec),
            'permitted_actions': gym.spaces.MultiBinary(num_actions)
        })

    # TODO: Connect to OCaml and adjust & evaluate AST
    def step(self, action):
        pass

    # TODO: Reset to original AST
    def reset(self):
        pass

    # TODO: Put a visual?
    def render(self, mode="human"):
        pass

    # TODO: Anything that needs to be cleaned up
    def close(self):
        pass
