import ctypes
import random
from typing import List

import gym
import numpy as np

max_num_nodes = 20
num_actions = 80
max_num_tests = 10
max_tree_length = 10000
max_num_vars = 10


class State(ctypes.Structure):
    _fields_ = [
        ("edges", (ctypes.c_int * (max_num_nodes**2)) * 3),
        ("tests", (ctypes.c_int * max_num_tests) * 2),
        ("nodes", ctypes.c_int * max_num_nodes),
        ("permitted_actions", ctypes.c_int * num_actions),
        ("vars_in_scope", ctypes.c_int * max_num_vars),
        ("zast", ctypes.c_char * max_tree_length),
        ("cursor", ctypes.c_int),
        ("num_nodes", ctypes.c_int),
        ("num_edges", ctypes.c_int),
        ("num_tests", ctypes.c_int),
        ("assignment", ctypes.c_int),
        ("code", ctypes.c_int),
    ]


class ASTEnv(gym.Env):
    def __init__(
        self,
        max_num_nodes: int,
        num_assignments: int,
        code_per_assignment: List[int],
        num_actions: int,
        max_num_tests: int = 10,
        max_tree_length: int = 10000,
        max_num_vars: int = 10,
    ):
        super(ASTEnv, self).__init__()

        State._fields_ = [
            ("edges", (ctypes.c_int * (max_num_nodes**2)) * 3),
            ("tests", (ctypes.c_int * max_num_tests) * 2),
            ("nodes", ctypes.c_int * max_num_nodes),
            ("permitted_actions", ctypes.c_int * num_actions),
            ("vars_in_scope", ctypes.c_int * max_num_vars),
            ("zast", ctypes.c_char * max_tree_length),
            ("cursor", ctypes.c_int),
            ("num_nodes", ctypes.c_int),
            ("num_edges", ctypes.c_int),
            ("num_tests", ctypes.c_int),
            ("assignment", ctypes.c_int),
            ("code", ctypes.c_int),
        ]

        # Set observation space
        num_node_descriptor = 10  # TODO: Specify this number
        node_nvec = num_node_descriptor * np.ones(max_num_nodes)
        edge_nvec = max_num_nodes * np.ones((max_num_nodes**2, 2))
        self.action_space = gym.spaces.Discrete(num_actions)
        self.observation_space = gym.spaces.Dict(
            {
                "nodes": gym.spaces.MultiDiscrete(node_nvec),
                "edges": gym.spaces.MultiDiscrete(edge_nvec),
                "permitted_actions": gym.spaces.MultiBinary(num_actions),
                "cursor_position": gym.spaces.Discrete(max_num_nodes),
                "vars_in_scope": gym.spaces.MultiDiscrete(max_num_nodes),
                "assignment": gym.spaces.Discrete(num_assignments),
            }
        )

        self.astclib = ctypes.CDLL("./clib/astclib.so")  # Used to call C functions
        self.state = None

        self.states = []

        self.astclib.init_c()

        for i in range(num_assignments):
            states = []
            for j in range(code_per_assignment[i]):
                state = State()
                self.astclib.init_assignment(
                    ctypes.byref(state), ctypes.c_int(i), ctypes.c_int(j)
                )
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

    def reset(self):
        assignment = self.observation_space.spaces["assignment"].sample()
        states = self.states[assignment]
        self.state = states[random.randint(0, len(states) - 1)]

        return self.get_state()

    def render(self):
        print("Current state:")
        self.astclib.print_curr_state(ctypes.byref(self.state))

    def close(self):
        self.astclib.close_c()

    # Get Python dictionary for self.state
    def get_state(self):
        return {
            "nodes": np.ctypeslib.as_array(self.state.nodes),
            "num_nodes": self.state.num_nodes,
            "edges": np.ctypeslib.as_array(self.state.edges).reshape(-1, 3),
            "num_edges": self.state.num_edges,
            "assignment": self.state.assignment,
            "cursor": self.state.cursor,
            "permitted_actions": np.ctypeslib.as_array(self.state.permitted_actions),
        }
