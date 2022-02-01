import gym
import numpy as np 
import ctypes


max_num_nodes = 10;
num_actions = 10;


class State(ctypes.Structure):
    _fields_ = [("nodes", ctypes.c_int * max_num_nodes),
                ("edges", (ctypes.c_int * (max_num_nodes ** 2)) * 2),
                ("permitted_actions", ctypes.c_int * num_actions)]


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
        
        self.astclib = ctypes.CDLL('clib/astlib.so')
        self.state = None

    def step(self, action):
        self.astclib.take_action(ctypes.byref(self.state), ctypes.c_int(action))
        reward = self.astclib.check_ast(ctypes.byref(self.state), ctypes.c_int(1)) # TODO: specify unit test index
        
        done = False
        if reward == 1:
            done = True
        else:
            self.astclib.valid_actions(ctypes.byref(self.state))
        
        return self.state, reward, done, {}

    # TODO: Reset to original AST
    def reset(self):
        self.state = State()
        self.astclib.get_ast(ctypes.byref(self.state), ctypes.c_int(1)) # TODO: specify which AST to get

    # TODO: Put a visual?
    def render(self, mode="human"):
        pass

    # TODO: Anything that needs to be cleaned up
    def close(self):
        pass
