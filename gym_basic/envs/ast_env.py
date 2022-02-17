import gym
import numpy as np 
import ctypes


max_num_nodes = 5;
num_actions = 5;


class State(ctypes.Structure):
    _fields_ = [("nodes", ctypes.c_int * max_num_nodes),
                ("edges", (ctypes.c_int * (max_num_nodes ** 2)) * 2),
                ("permitted_actions", ctypes.c_int * num_actions)]


class ASTEnv(gym.Env):
    def __init__(self):
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
        
        self.astclib = ctypes.CDLL('clib/astclib.so') # Used to call C functions
        self.state = None # Current state of environment
        
        self.astclib.init_c()

    def step(self, action):
        self.astclib.take_action(ctypes.byref(self.state), ctypes.c_int(action))
        reward = self.astclib.check_ast(ctypes.byref(self.state), ctypes.c_int(action)) # TODO: specify unit test index
        
        done = False
        if reward == 1:
            done = True
        else:
            self.astclib.valid_actions(ctypes.byref(self.state))
        
        # Change state to Python dict
        state = {'nodes': np.ctypeslib.as_array(self.state.nodes), 
                'edges': np.ctypeslib.as_array(self.state.edges).reshape(-1, 2), 
                'permitted_actions': np.ctypeslib.as_array(self.state.permitted_actions)}
        
        return state, reward, done, {}

    # TODO: Reset to original AST
    def reset(self):
        self.state = State()
        self.astclib.get_ast(ctypes.byref(self.state), ctypes.c_int(1)) # TODO: specify which AST to get
        
        # Change state to Python dict
        state = {'nodes': np.ctypeslib.as_array(self.state.nodes), 
                'edges': np.ctypeslib.as_array(self.state.edges).reshape(-1, 2), 
                'permitted_actions': np.ctypeslib.as_array(self.state.permitted_actions)}
        return state
        
    # # TODO: Put a visual?
    def render(self, mode="human"):
        state = {'nodes': np.ctypeslib.as_array(self.state.nodes), 
                'edges': np.ctypeslib.as_array(self.state.edges).reshape(-1, 2), 
                'permitted_actions': np.ctypeslib.as_array(self.state.permitted_actions)}
        
        print("Current environment:")
        print(f"\tNodes: {state['nodes']}")
        print("\tEdges: ", end='')
        for edge in state['edges']:
            if (edge[0] != edge[1]):
                print(edge, end='')
        print()
        print(f"\tPermitted Actions: {state['permitted_actions']}")

    # TODO: Anything that needs to be cleaned up
    def close(self):
        self.astclib.close_c()
