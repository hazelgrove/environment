from collections import defaultdict
from typing import List

import gym
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch_geometric.nn as gnn
from gym.spaces.utils import unflatten
import copy

from agent.base import CNNBase, GNNBase, MLPBase
from agent.distributions import Bernoulli, Categorical, CategoricalAction, DiagGaussian
from agent.utils import batch_unflatten, init


class Policy(nn.Module):
    def __init__(self, obs_space, action_space, base=None, base_kwargs=None):
        super(Policy, self).__init__()
        if base_kwargs is None:
            base_kwargs = {}
        if base is None:
            if len(obs_space.shape) == 3:
                base = CNNBase
            elif len(obs_space.shape) == 1:
                base = MLPBase
            else:
                raise NotImplementedError

        self.base = base(obs_space.shape[0], **base_kwargs)

        if action_space.__class__.__name__ == "Discrete":
            num_outputs = action_space.n
            self.dist = Categorical(self.base.output_size, num_outputs)
        elif action_space.__class__.__name__ == "Box":
            num_outputs = action_space.shape[0]
            self.dist = DiagGaussian(self.base.output_size, num_outputs)
        elif action_space.__class__.__name__ == "MultiBinary":
            num_outputs = action_space.shape[0]
            self.dist = Bernoulli(self.base.output_size, num_outputs)
        else:
            raise NotImplementedError

    @property
    def is_recurrent(self):
        return self.base.is_recurrent

    @property
    def recurrent_hidden_state_size(self):
        """Size of rnn_hx."""
        return self.base.recurrent_hidden_state_size

    def forward(self, inputs, rnn_hxs, masks):
        raise NotImplementedError

    def act(self, inputs, rnn_hxs, masks, deterministic=False):
        value, actor_features, rnn_hxs = self.base(inputs, rnn_hxs, masks)
        dist = self.dist(actor_features)

        if deterministic:
            action = dist.mode()
        else:
            action = dist.sample()

        action_log_probs = dist.log_probs(action)

        return value, action, action_log_probs, rnn_hxs

    def get_value(self, inputs, rnn_hxs, masks):
        value, _, _ = self.base(inputs, rnn_hxs, masks)
        return value

    def evaluate_actions(self, inputs, rnn_hxs, masks, action):
        value, actor_features, rnn_hxs = self.base(inputs, rnn_hxs, masks)
        dist = self.dist(actor_features)

        action_log_probs = dist.log_probs(action)
        dist_entropy = dist.entropy().mean()

        return value, action_log_probs, dist_entropy, rnn_hxs


class GNNPolicy(Policy):
    def __init__(self, obs_space, action_space, base_kwargs=None):
        super(Policy, self).__init__()

        self.obs_space = obs_space

        if base_kwargs is None:
            base_kwargs = {}
        self.base = GNNBase(**base_kwargs)

        num_outputs = action_space.n
        self.dist = CategoricalAction(self.base.output_size, num_outputs)
        
    def __unpad_states(self, state):
        breakpoint()
        for i in range(state["nodes"].shape[0]):
            if state["nodes"][i] == -1:
                state["nodes"] = state["nodes"][:i]
                break

        for i in range(state["edges"].shape[0]):
            if state["edges"][i][0] == -1:
                state["edges"] = state["edges"][:i]
                break

        for i in range(state["vars_in_scope"].shape[0]):
            if state["vars_in_scope"][i] == -1:
                state["vars_in_scope"] = state["vars_in_scope"][:i]
                break

        return state

    def __unflatten_input(self, input):
        input_dict = defaultdict(list)

        for i in range(input.shape[0]):
            datum = unflatten(self.obs_space, np.array(input[i]))
            datum = self.__unpad_states(datum)

            for key, value in datum.items():
                input_dict[key].append(value)

        for key in input_dict.keys():
            input_dict[key] = torch.tensor(np.array(input_dict[key]))
        
        return input_dict
    
    def __batch_unflatten(self, inputs):
        breakpoint()
        if inputs.dim() == 2:
            batch_size = inputs.shape[0]
        else:
            batch_size = 1
        
        if batch_size != 1:
            inputs = self.__unflatten_input(inputs)
            inputs["edge_index"], inputs["edge_attr"] = inputs["edges"][:, :, :2].transpose(1, 2), inputs["edges"][:, :, 2]
        else:
            inputs = unflatten(self.obs_space, inputs)
            inputs["edge_index"], inputs["edge_attr"] = inputs["edges"][:, :2].reshape(2, -1), inputs["edges"][:, 2]
        
        return inputs
        
    def act(self, inputs, rnn_hxs, masks, deterministic=False):
        inputs = self.__batch_unflatten(inputs)
        breakpoint()
        value, actor_features = self.base(x=inputs["nodes"], 
                                                   edge_index=inputs["edge_index"], 
                                                   edge_attr=inputs["edge_attr"], 
                                                   assignment=inputs["assignment"], 
                                                   cursor=inputs["cursor_position"])
        
        dist = self.dist(actor_features, inputs["permitted_actions"])

        if deterministic:
            action = dist.mode()
        else:
            action = dist.sample()

        action_log_probs = dist.log_probs(action)

        return value, action, action_log_probs, rnn_hxs
    
    def get_value(self, inputs, rnn_hxs, masks):
        inputs = self.__batch_unflatten(inputs)
        value, _ = self.base(x=inputs["nodes"], 
                                                   edge_index=inputs["edge_index"], 
                                                   edge_attr=inputs["edge_attr"], 
                                                   assignment=inputs["assignment"], 
                                                   cursor=inputs["cursor_position"])        
        return value

    def evaluate_actions(self, inputs, rnn_hxs, masks, action):
        inputs = self.__batch_unflatten(inputs)
        value, actor_features = self.base(x=inputs["nodes"], 
                                                   edge_index=inputs["edge_index"], 
                                                   edge_attr=inputs["edge_attr"], 
                                                   assignment=inputs["assignment"], 
                                                   cursor=inputs["cursor_position"])
        dist = self.dist(actor_features)

        action_log_probs = dist.log_probs(action)
        dist_entropy = dist.entropy().mean()

        return value, action_log_probs, dist_entropy, rnn_hxs
