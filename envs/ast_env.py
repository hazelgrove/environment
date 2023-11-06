import copy
import os
import ctypes
import random
from itertools import product
from typing import Any, List, Optional, Tuple, TypedDict, TypeVar, Union

import gym
import numpy as np
import numpy.typing as npt


class State(ctypes.Structure):
    pass


class ASTEnv(gym.Env):
    def __init__(
        self,
        max_num_nodes: int,
        num_node_descriptor: int,
        num_assignments: int,
        code_per_assignment: List[int],
        num_actions: int,
        assignment_dir: str,
        perturbation: int = 0,
        max_num_tests: int = 10,
        max_tree_length: int = 10000,
        max_num_vars: int = 11,
        seed: int = 0,
        cursor_start_pos: Optional[int] = None,
        curriculum: Optional[Union[List[int], int]] = None,
        curriculum_threshold: Optional[int] = None,
        done_action:bool = False,
        ds_ratio:List[bool] = [],
        multi_ds:bool=False,
        max_episode_steps_per_ds=None
    ):
        super(ASTEnv, self).__init__()

        State._fields_ = [
            ("edges", (ctypes.c_int * (max_num_nodes * 3)) * 3),
            ("tests", (ctypes.c_int * max_num_tests) * 2),
            ("nodes", ctypes.c_int * max_num_nodes),
            ("starter", ctypes.c_int * max_num_nodes),
            ("permitted_actions", ctypes.c_int * (num_actions + max_num_vars * 2)),
            ("vars_in_scope", ctypes.c_int * max_num_vars),
            ("args_in_scope", ctypes.c_int * max_num_vars * 2),
            ("zast", ctypes.c_char * max_tree_length),
            ("cursor", ctypes.c_int),
            ("num_nodes", ctypes.c_int),
            ("num_edges", ctypes.c_int),
            ("num_tests", ctypes.c_int),
            ("num_vars", ctypes.c_int),
            ("num_args", ctypes.c_int),
            ("assignment", ctypes.c_int),
            ("code", ctypes.c_int),
        ]

        # Set action and observation space
        self.num_node_descriptor = num_node_descriptor
        self.max_num_nodes = max_num_nodes
        self.num_actions = num_actions
        self.max_num_vars = max_num_vars
        self.perturbation = perturbation
        # done action info
        self.done_action = done_action

        # for logging and sampling 
        if multi_ds: 
            self.dataset_inds = [
                [
                    (assn,test) 
                    for assn, n_tests in enumerate(code_per_assignment[ds_ind]) 
                    for test in range(n_tests) 
                ]
                for ds_ind in range(len(ds_ratio))
            ]

        else:
            self.dataset_inds = [
                (assn,test) 
                    for assn, n_tests in enumerate(code_per_assignment) 
                    for test in range(n_tests) 
            ]
        self.assignment_no = None 
        self.problem_no = None 
        self.num_nodes_list = []

        # Plus one to account for -1
        node_nvec = (num_node_descriptor + max_num_vars * 2 + 1) * np.ones(
            max_num_nodes
        )
        edge_nvec = (max_num_nodes + 1) * np.ones((max_num_nodes * 3, 3))
        vars_nvec = (max_num_nodes + 1) * np.ones(max_num_vars)
        args_nvec = (max_num_nodes + 1) * np.ones((max_num_vars, 2))
    
        num_possible_agent_actions = num_actions + max_num_vars +1 if done_action else num_actions + max_num_vars
        permitted_action_size = num_actions + max_num_vars * 2 +1 if done_action else num_actions + max_num_vars * 2
        self.done_action_num = -1 # Make done action -1 rather than max; set this to -1 when done


        # print(done_action, num_possible_agent_actions, permitted_action_size, num_actions, max_num_vars)


        self.action_space = gym.spaces.Discrete(num_possible_agent_actions)
        self.observation_space = gym.spaces.Dict(
            {
                "nodes": gym.spaces.MultiDiscrete(node_nvec),
                "edges": gym.spaces.MultiDiscrete(edge_nvec),
                "permitted_actions": gym.spaces.MultiBinary(
                    permitted_action_size
                ),
                "starter": gym.spaces.MultiDiscrete(node_nvec),
                "cursor_position": gym.spaces.Discrete(max_num_nodes),
                "vars_in_scope": gym.spaces.MultiDiscrete(vars_nvec),
                "args_in_scope": gym.spaces.MultiDiscrete(args_nvec),
                "assignment": gym.spaces.Discrete(sum(num_assignments)),
            }
        )

        self.astclib = ctypes.CDLL(
            "/RL_env/clib/astclib.so"
        )  # Used to call C functions
        self.state = None

        self.code_per_assignment = code_per_assignment
        self.assignment_dir = assignment_dir
        self.cursor_start_pos = cursor_start_pos
        self.multi_ds = multi_ds
        self.ds_ratio = ds_ratio
        self.curriculum = curriculum
        self.max_episode_steps_per_ds = max_episode_steps_per_ds

        self.curriculum_threshold = curriculum_threshold
        if self.curriculum is not None and self.curriculum_threshold is None:
            raise ValueError("Curriculum threshold must be set if curriculum is set")
        if self.curriculum_threshold is not None and self.curriculum is None:
            raise ValueError("Curriculum must be set if curriculum threshold is set")
        self.curriclum_index = 1

        self.astclib.init_c(ctypes.c_int(seed))
        self.random = np.random.default_rng(seed=seed)

    def step(self, action: int):
        if self.done_action:
            # we've added an additional element in position 0, correct for it 
            action -=1 

        truncated=False

        if not (self.done_action and action == self.done_action_num): # first (-1 th) action --> dummy action
            # set action to move parent (doesnt change actual structrue)
            try: 
                self.astclib.take_action(ctypes.byref(self.state), ctypes.c_int(action))
            except EOFError: 
                print(f'error occurred taking action in assn# {self.assignment_no}, problem # {self.problem_no}')
                print(self.get_state())
                print(f'action was {action}') 
                print(self.num_nodes_list )
                raise EOFError
        try: 
            reward = self.astclib.check_ast(ctypes.byref(self.state))
        except EOFError: 
            print(f'error occurred taking action in assn# {self.assignment_no}, problem # {self.problem_no}')
            print(self.get_state())
            print(f'action was {action}') 
            raise EOFError


        done = False 
        if  self.done_action:
            done = (action == self.done_action_num)
            reward =  1 if reward and done  == 1 else 0 
        elif reward == 1:
            done=True

        # Change state to Python dict
        state = self.get_state()

        # If we create too many nodes, this is an error. 
        # If we reach max_num_nodes-1 nodes we are in danger of crashing 
        # instead, if we reach that many nodes (we shouldn't), return as a failed run. 
        self.num_nodes_list.append(self.state.num_nodes)
        if self.state.num_nodes >= self.max_num_nodes -5: 
            done = True
            truncated = True
            reward = 0 
            print('MAX NUMBER OF NODES EXCEEDED')
            self.render()


        infos = {}
        if self.multi_ds:
            self.curr_step +=1 
            if self.curr_step == self.max_episode_steps_per_ds: 
                # finish; 
                done=True
            infos = {'ds_num':self.ds_num}

        return state, reward, done, truncated, infos

    def reset(self,seed=None):
        if seed is None: 
            seed = int(self.random.integers(2**60))
        super().reset(seed=seed)
        if self.multi_ds:
            ds_num = self.random.choice(list(range(len(self.ds_ratio))),p=self.ds_ratio)
            assignment, code = self.random.choice(self.dataset_inds[ds_num])
            assignment_dir = self.assignment_dir[ds_num]
            self.ds_num = ds_num 
            # print(assignment,code,assignment_dir)
        elif self.curriculum is not None:
            assignment_dir = self.assignment_dir
            assignment = self.random.choice(self.curriculum[: self.curriculum_index])[0]
            code = self.random.randint(0, self.code_per_assignment[assignment] - 1)
        else:
            assignment_dir = self.assignment_dir
            # assignment = self.observation_space.spaces["assignment"].sample()
            assignment, code = self.random.choice(self.dataset_inds,k=1)[0]

        # with open(os.path.join(assignment_dir,str(int(assignment)),f'{int(code)}.ml'),'r') as file: 
        #     print(file.read())
        
             
        self.assignment_no = assignment
        self.problem_no = code
        self.num_nodes_list = []
        self.max_num_actions = self.max_episode_steps_per_ds[ds_num]
        self.curr_step = 0 

        self.state = State()
        
        if self.cursor_start_pos is None:
            # print(code,assignment, code,self.assignment_dir)
            self.astclib.init_assignment(
                ctypes.byref(self.state),
                bytes(assignment_dir, encoding="utf8"),
                ctypes.c_int(assignment),
                ctypes.c_int(code),
                ctypes.c_int(self.perturbation),
                ctypes.c_int(-1),
            )
        else:
            # print(self.cursor_start_pos[ds_num][assignment],code,assignment, code,self.assignment_dir[ds_num])
            self.astclib.init_assignment(
                ctypes.byref(self.state),
                bytes(self.assignment_dir[ds_num], encoding="utf8"),
                ctypes.c_int(assignment),
                ctypes.c_int(code),
                ctypes.c_int(self.perturbation),
                ctypes.c_int(self.cursor_start_pos[ds_num][assignment]),
            )
        
        infos = {'ds_num':self.ds_num}

        return self.get_state(), infos

    def render(self, mode=None) -> None:
        print("Current state:")
        try: 
            self.astclib.print_curr_state(ctypes.byref(self.state))
        except EOFError: 
            print(f'error occurred in assn# {self.assignment_no}, problem # {self.problem_no}')
            print(self.get_state())
            raise EOFError


    def close(self) -> None:
        self.astclib.close_c()

    def update_curriculum(self, reward: float):
        if self.curriculum is None:
            return
        if reward >= self.curriculum_threshold and self.curriculum_index < len(
            self.curriculum
        ):
            self.curriclum_index += 1

    def update_curriculum(self, reward: float):
        if self.curriculum is None:
            return
        if reward >= self.curriculum_threshold and self.curriculum_index < len(
            self.curriculum
        ):  
            self.curriclum_index += 1

    # Get Python dictionary for self.state
    def get_state(self):
        permitted_actions = np.ctypeslib.as_array(self.state.permitted_actions)
        if self.done_action: 
            permitted_actions = np.append([1],permitted_actions) # make 1st spot the 'always possible' 'done' action 

        state = {
            "nodes": np.ctypeslib.as_array(self.state.nodes),
            "edges": np.ctypeslib.as_array(self.state.edges).reshape(-1, 3),
            "starter": np.ctypeslib.as_array(self.state.starter),
            "permitted_actions": permitted_actions,
            "cursor_position": self.state.cursor,
            "vars_in_scope": np.ctypeslib.as_array(self.state.vars_in_scope),
            "args_in_scope": np.ctypeslib.as_array(self.state.args_in_scope).reshape(
                -1, 2
            ),
            "assignment": self.state.assignment,
        }

        return self.pad_states(state)

    def pad_states(self, state):
        for i in range(self.state.num_nodes, self.max_num_nodes):
            state["nodes"][i] = -1
            state["starter"][i] = -1

        for i in range(self.state.num_edges, self.max_num_nodes * 3):
            state["edges"][i][0] = -1

        for i in range(self.state.num_vars, self.max_num_vars):
            state["vars_in_scope"][i] = -1

        for i in range(self.state.num_args, self.max_num_vars):
            state["args_in_scope"][i][0] = -1

        return state

    def unpad_states(self, state):
        for i in range(self.max_num_nodes):
            if state["nodes"][i] == -1:
                state["nodes"] = state["nodes"][:i]
                state["starter"] = state["starter"][:i]
                break

        for i in range(self.max_num_nodes * 3):
            if state["edges"][i][0] == -1:
                state["edges"] = state["edges"][:i]
                break

        for i in range(self.max_num_vars):
            if state["vars_in_scope"][i] == -1:
                state["vars_in_scope"] = state["vars_in_scope"][:i]
                break

        for i in range(self.max_num_vars):
            if state["args_in_scope"][i][0] == -1:
                state["args_in_scope"] = state["args_in_scope"][:i]
                break

        return state
