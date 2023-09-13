from dataclasses import asdict, astuple

import gym
import numpy as np
import torch
import torch.nn as nn
from gym.spaces import Discrete, MultiBinary, MultiDiscrete
from transformers import GraphormerForGraphClassification, GraphormerConfig

from agent.base import CNNBase, GNNBase, MLPBase, TestBase
from agent.distributions import (
    QKV,
    Bernoulli,
    Categorical,
    DiagGaussian,
    MaskedCategorical,
)
from agent.wrapper import Obs


def get_size(space):
    if isinstance(space, Discrete):
        return 1
    elif isinstance(space, (MultiDiscrete, MultiBinary)):
        return int(np.prod(space.shape))
    else:
        raise NotImplementedError


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
        # print('initing policy')
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
    def __init__(
        self,
        obs_space,
        action_space,
        num_fixed_actions,
        max_num_vars=11,
        base_kwargs=None,
        device=None,
        done_action=False,
        use_qkv=True
    ):
        super(Policy, self).__init__()
        self.has_done_action = done_action
        
        self.use_qkv =  use_qkv = base_kwargs.pop('use_qkv',use_qkv)

        # handle number of actions... we have one extra if we're using a done action 
        self.num_nonvar_actions = num_fixed_actions + 1 if done_action else num_fixed_actions
        self.num_actions = self.num_nonvar_actions +  max_num_vars *2

        self.obs_space = Obs(**obs_space.spaces)

        if base_kwargs is None:
            base_kwargs = {}
        self.base = GNNBase(device=device, **base_kwargs)
            
        # use qkv by default, else  use a projection 
        if self.use_qkv: 
            self.qkv = QKV(
                num_fixed_actions=self.num_nonvar_actions,
                embedding_size=self.base.output_size,
                max_num_vars=max_num_vars,
            )
        else: 
            self.projection = torch.nn.Linear(self.base.hidden_size,self.num_actions)
            # 

        self.dist = MaskedCategorical()
        self.device = device
    
    def _get_dist(self, inputs):
        inputs = Obs(
            *torch.split(
                inputs,
                [get_size(space) for space in astuple(self.obs_space)],
                dim=-1,
            )
        )
        value, actor_features, vars = self.base(asdict(inputs))

        args_in_scope = inputs.args_in_scope.reshape( # what does this do ????? 
            inputs.args_in_scope.shape[0], -1, 2
        )
        if self.use_qkv: 
            # If we choose not to use qkv, instead use a simple learned projection
            actor_features = self.qkv(actor_features, vars, args_in_scope)
        else: 
            # If we choose not to use qkv, instead use a simple learned projection
            actor_features = self.projection(actor_features)
        dist = self.dist(actor_features, inputs.permitted_actions)

        return dist, value

    def act(self, inputs, rnn_hxs, masks, deterministic=False):
        dist,value = self._get_dist(inputs)
        if deterministic:
            action = dist.mode()
        else:
            action = dist.sample()

        action_log_probs = dist.log_probs(action)

        return value, action, action_log_probs, rnn_hxs
    

    def get_value(self, inputs, rnn_hxs, masks):
        _ ,value = self._get_dist(inputs)

        return value

    def evaluate_actions(self, inputs, rnn_hxs, masks, action):
        dist,value = self._get_dist(inputs)

        action_log_probs = dist.log_probs(action)
        dist_entropy = dist.entropy().mean()

        return value, action_log_probs, dist_entropy, rnn_hxs
    
    
class TestPolicy(Policy):
    def __init__(
        self,
        base_kwargs=None,
        device=None,
    ):
        super(Policy, self).__init__()

        if base_kwargs is None:
            base_kwargs = {}
        self.base = TestBase(device=device, **base_kwargs)
        
        self.dist = Categorical(self.base.output_size, 2)

        self.device = device

    def act(self, inputs, rnn_hxs, masks, deterministic=False):
        value, actor_features = self.base(inputs)

        dist = self.dist(actor_features)

        if deterministic:
            action = dist.mode()
        else:
            action = dist.sample()

        action_log_probs = dist.log_probs(action)

        return value, action, action_log_probs, rnn_hxs

    def get_value(self, inputs, rnn_hxs, masks):
        value, _ = self.base(inputs)

        return value

    def evaluate_actions(self, inputs, rnn_hxs, masks, action):
        value, actor_features = self.base(inputs)

        dist = self.dist(actor_features)

        action_log_probs = dist.log_probs(action)
        dist_entropy = dist.entropy().mean()

        return value, action_log_probs, dist_entropy, rnn_hxs


class GraphphormerPolicy(GNNPolicy):
    # graphphormer. Override init and _get_dist
    def __init__(
        self,
        obs_space,
        action_space,
        num_fixed_actions,
        max_num_vars=11,
        base_kwargs={},
        device=None,
        done_action=False,
    ):
        super().__init__()
        self.has_done_action = done_action
        

        # handle number of actions... we have one extra if we're using a done action 
        self.num_nonvar_actions = num_fixed_actions + 1 if done_action else num_fixed_actions
        self.num_actions = self.num_nonvar_actions +  max_num_vars *2

        self.obs_space = Obs(**obs_space.spaces)
        num_edge_descriptor = base_kwargs.get('num_edge_descriptor',  7)
        max_num_vars = base_kwargs.get('max_num_vars', 11)
        num_node_descriptor = base_kwargs.get('num_node_descriptor', 107)

        model_config ={
            "num_classes": self.num_actions,
            "num_atoms": num_node_descriptor + max_num_vars * 2 + 1,
            "num_edges": num_edge_descriptor,
            "num_hidden_layers": base_kwargs.get('num_layers',8),
            "embedding_dim": base_kwargs.get('num_layers',768),
            "ffn_embedding_dim": base_kwargs.get('num_layers',768),
            "num_attention_heads": base_kwargs["heads"] if base_kwargs and "heads" in base_kwargs else 32,
        }
        self.base = GraphormerForGraphClassification(
                GraphormerConfig(model_config)
            )
    
        self.dist = MaskedCategorical()
        self.device = device
    
    def _get_dist(self, inputs):
        inputs = Obs(
            *torch.split(
                inputs,
                [get_size(space) for space in astuple(self.obs_space)],
                dim=-1,
            )
        )
        # in_vals = 
        # input_nodes: torch.LongTensor,
        # input_edges: torch.LongTensor,
        # attn_bias: torch.Tensor,
        # in_degree: torch.LongTensor,
        # out_degree: torch.LongTensor,
        # spatial_pos: torch.LongTensor,
        # attn_edge_type: torch.LongTensor,
        value, actor_features, vars = self.base(asdict(inputs))

        args_in_scope = inputs.args_in_scope.reshape( # what does this do ????? 
            inputs.args_in_scope.shape[0], -1, 2
        )
        if self.use_qkv: 
            # If we choose not to use qkv, instead use a simple learned projection
            actor_features = self.qkv(actor_features, vars, args_in_scope)
        else: 
            # If we choose not to use qkv, instead use a simple learned projection
            actor_features = self.projection(actor_features)
        dist = self.dist(actor_features, inputs.permitted_actions)

        return dist, value

    @property
    def recurrent_hidden_state_size(self):
        """Size of rnn_hx."""
        return 1
