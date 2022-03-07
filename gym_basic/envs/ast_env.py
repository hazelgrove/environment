import gym
import numpy as np 
import ctypes
import random


max_num_nodes = 10
num_actions = 5
max_num_tests = 10


class State(ctypes.Structure):
    _fields_ = [("edges", (ctypes.c_int * (max_num_nodes ** 2)) * 3),
                ("tests", (ctypes.c_int * max_num_tests) * 2),
                ("nodes", ctypes.c_int * max_num_nodes),
                ("permitted_actions", ctypes.c_int * num_actions),
                ("root", ctypes.c_int),
                ("num_nodes", ctypes.c_int),
                ("num_edges", ctypes.c_int),
                ("num_tests", ctypes.c_int),
                ("assignment", ctypes.c_int),
                ("code", ctypes.c_int)
                ]


class ASTEnv(gym.Env):
    def __init__(self):
        super(ASTEnv, self).__init__()

        self.action_space = None
        self.observation_space = None
        
        self.astclib = ctypes.CDLL('clib/astclib.so') # Used to call C functions
        self.state = None
        
        self.states = []
        
        self.astclib.init_c()
        
    def init(self, num_assignments, code_per_assignment):
        # Set observation space
        num_node_descriptor = 10 # TODO: Specify this number
        node_nvec = num_node_descriptor * np.ones(max_num_nodes)
        edge_nvec = max_num_nodes * np.ones((max_num_nodes ** 2, 2))
        self.action_space = gym.spaces.Discrete(num_actions)
        self.observation_space = gym.spaces.Dict({
            'nodes': gym.spaces.MultiDiscrete(node_nvec),
            'edges': gym.spaces.MultiDiscrete(edge_nvec),
            'permitted_actions': gym.spaces.MultiBinary(num_actions),
            'assignment': gym.spaces.Discrete(num_assignments)
        })
        
        for i in range(num_assignments):
            states = []
            for j in range(code_per_assignment[i]):
                state = State()
                self.astclib.init_assignment(ctypes.byref(state), ctypes.c_int(i), ctypes.c_int(j))
                states.append(state)
            self.states.append(states)

    def step(self, action):
        self.astclib.take_action(ctypes.byref(self.state), ctypes.c_int(action))
        reward = self.astclib.check_ast(ctypes.byref(self.state))
        
        done = False
        if reward == 1:
            done = True
        
        # Change state to Python dict
        state = self.get_state()
        
        return state, reward, done, {}

    # TODO: Reset to original AST
    def reset(self):
        assignment = self.observation_space.spaces['assignment'].sample()
        states = self.states[assignment]
        self.state = states[random.randint(0, len(states) - 1)]
        
        # Change state to Python dict
        state = self.get_state()
        return state
        
    # # TODO: Put a visual?
    def render(self, mode="human"):
        state = self.get_state()
        
        print("Current environment:")
        print("\tNodes: ", end='')
        for i in range(state['num_nodes']):
            print(state['nodes'][i], end=' ')
        print()
        print("\tEdges: ", end='')
        for i in range(state['num_edges']):
            print(state['edges'][i], end=' ')
        print()

    # TODO: Anything that needs to be cleaned up
    def close(self):
        self.astclib.close_c()
        
    def get_state(self):
        return {'nodes': np.ctypeslib.as_array(self.state.nodes), 
                'num_nodes': self.state.num_nodes,
                'edges': np.ctypeslib.as_array(self.state.edges).reshape(-1, 3),
                'num_edges': self.state.num_edges}