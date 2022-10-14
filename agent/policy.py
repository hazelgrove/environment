from dataclasses import asdict, astuple

import gym
import numpy as np
import torch
import torch.nn as nn
from gym.spaces import Discrete, MultiBinary, MultiDiscrete

from agent.base import CNNBase, GNNBase, MLPBase
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
    def __init__(
        self,
        obs_space,
        action_space,
        num_fixed_actions,
        max_num_vars=11,
        base_kwargs=None,
        device=None,
    ):
        super(Policy, self).__init__()

        self.obs_space = Obs(**obs_space.spaces)

        if base_kwargs is None:
            base_kwargs = {}
        self.base = GNNBase(device=device, **base_kwargs)

        self.qkv = QKV(
            num_fixed_actions=num_fixed_actions,
            embedding_size=self.base.output_size,
            max_num_vars=max_num_vars,
        )
        self.dist = MaskedCategorical(
            self.base.output_size, num_fixed_actions + 2 * max_num_vars
        )
        self.device = device

    def act(self, inputs, rnn_hxs, masks, deterministic=False):
        inputs = Obs(
            *torch.split(
                inputs,
                [get_size(space) for space in astuple(self.obs_space)],
                dim=-1,
            )
        )
        value, actor_features, vars = self.base(asdict(inputs))

        args_in_scope = inputs.args_in_scope.reshape(
            inputs.args_in_scope.shape[0], -1, 2
        )
        actor_features = self.qkv(actor_features, vars, args_in_scope)
        dist = self.dist(actor_features, inputs.permitted_actions)

        if deterministic:
            action = dist.mode()
        else:
            action = dist.sample()

        action_log_probs = dist.log_probs(action)

        return value, action, action_log_probs, rnn_hxs

    def get_value(self, inputs, rnn_hxs, masks):
        inputs = Obs(
            *torch.split(
                inputs,
                [get_size(space) for space in astuple(self.obs_space)],
                dim=-1,
            )
        )
        value, _, _ = self.base(asdict(inputs))

        return value

    def evaluate_actions(self, inputs, rnn_hxs, masks, action):
        inputs = Obs(
            *torch.split(
                inputs,
                [get_size(space) for space in astuple(self.obs_space)],
                dim=-1,
            )
        )
        value, actor_features, vars = self.base(asdict(inputs))

        args_in_scope = inputs.args_in_scope.reshape(
            inputs.args_in_scope.shape[0], -1, 2
        )
        actor_features = self.qkv(actor_features, vars, args_in_scope)
        dist = self.dist(actor_features, inputs.permitted_actions)

        action_log_probs = dist.log_probs(action)
        dist_entropy = dist.entropy().mean()

        return value, action_log_probs, dist_entropy, rnn_hxs
