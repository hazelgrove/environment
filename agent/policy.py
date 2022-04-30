import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch_geometric.nn as gnn

from agent.distributions import Bernoulli, Categorical, DiagGaussian
from agent.utils import init


class Flatten(nn.Module):
    def forward(self, x):
        return x.view(x.size(0), -1)


# Our NN model
class GNNBase(nn.Module):
    def __init__(self):
        super(GNNBase, self).__init__()
        self.hid_1 = 128
        self.in_head = 8
        self.hid_2 = 64
        self.hid_head = 8
        self.out = 32
        self.out_head = 1
        self.in_edge_channels = 10

        self.conv1 = gnn.GeneralConv(
            1035,
            self.hid_1,
            in_edge_channels=self.in_edge_channels,
            directed_msg=True,
            attention=True,
            heads=self.in_head,
        )
        self.conv2 = gnn.GeneralConv(
            self.hid_1,
            self.hid_2,
            in_edge_channels=self.in_edge_channels,
            directed_msg=True,
            attention=True,
            heads=self.hid_head,
        )
        self.conv3 = gnn.GeneralConv(
            self.hid_2,
            self.out,
            in_edge_channels=self.in_edge_channels,
            directed_msg=True,
            attention=True,
            heads=16,
        )
        self.extra_conv = gnn.GeneralConv(
            self.out,
            2,
            in_edge_channels=self.in_edge_channels,
            directed_msg=True,
            attention=True,
            heads=1,
        )

    def forward(self, data):
        x, edge_index, edge_attr = data.x, data.edge_index, data.edge_attr
        x = x.float()
        # print(x.size())
        x = self.conv1(x, edge_index, edge_feature=edge_attr)
        # print(x.size())
        x = F.elu(x)
        # print(x.size())
        x = F.dropout(x, p=0.4, training=self.training)
        x = self.conv2(x, edge_index, edge_feature=edge_attr)
        x = F.elu(x)
        x = F.dropout(x, p=0.4, training=self.training)
        x = self.conv3(x, edge_index, edge_feature=edge_attr)
        x = F.elu(x)

        z = self.extra_conv(x, edge_index)
        return z


class Policy(nn.Module):
    def __init__(self, obs_shape, action_space, base=GNNBase, base_kwargs=None):
        super(Policy, self).__init__()
        if base_kwargs is None:
            base_kwargs = {}

        self.base = base(obs_shape[0], **base_kwargs)

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
