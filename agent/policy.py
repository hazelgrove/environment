from typing import List
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch_geometric.nn as gnn

from agent.distributions import CategoricalAction
from agent.utils import init


class Flatten(nn.Module):
    def forward(self, x):
        return x.view(x.size(0), -1)


# Our NN model
class GNNBase(nn.Module):
    def __init__(
        self,
        gnn_layer_size: List[int] = [1035, 128, 64, 32],
        heads: List[int] = [8, 8, 16, 1],
        in_edge_channels: int = 10,
        linear_hidden_size: int = 512,
        num_node_descriptor: int = 50,
        embedding_dim: int = 512,
    ):
        super(GNNBase, self).__init__()
        
        self.gnn_layer_size = gnn_layer_size
        self.heads = heads
        self.in_edge_channels = in_edge_channels
        self.linear_hidden_size = linear_hidden_size
        
        self.main = gnn.Sequential('x, edge_index, edge_feature', [
            (gnn.GeneralConv(
                self.hidden_layer_size[0], 
                self.hidden_layer_size[1],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[0],
            ), 'x, edge_index, edge_feature -> x'),
            nn.ELU(),
            nn.Dropout(p=0.4),
            (gnn.GeneralConv(
                self.hidden_layer_size[1], 
                self.hidden_layer_size[2],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[1],
            ), 'x, edge_index, edge_feature -> x'),
            nn.ELU(),
            nn.Dropout(p=0.4),
            (gnn.GeneralConv(
                self.hidden_layer_size[2], 
                self.hidden_layer_size[3],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[2],
            ), 'x, edge_index, edge_feature -> x'),
            nn.ELU(),
            (gnn.GeneralConv(
                self.hidden_layer_size[3],
                self.hidden_layer_size[4],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[3],
            ), 'x, edge_index -> x')
        ])
        
        # TODO: Set embedding size
        self.node_embedding = nn.Embedding(num_node_descriptor, embedding_dim)
        
        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )
        self.critic_linear = init_(nn.Linear(self.linear_hidden_size, 1))
        
        self.train()

    def forward(self, data):
        #TODO: Add / Concatenate assignment index to node/edge embedding.
        #TODO: only want info on cursor node
        #TODO: embedding for edge types
        
        #TODO: edge_index to long
        
        x, edge_index, edge_attr = data["nodes"].reshape((-1, 1)), data["edges"], data["edge-type"].reshape((-1, 1))
        x = x.long()
        x = self.node_embedding(x)
        
        x = self.main(x, edge_index, edge_attr)
        
        return self.critic_linear(x), x


class Policy(nn.Module):
    def __init__(self, action_space, base=None, base_kwargs=None):
        # TODO: Add env here
        
        super(Policy, self).__init__()
        
        if base_kwargs is None:
            base_kwargs = {}
        if base is None:
            base = GNNBase
        
        self.base = base(**base_kwargs)
        
        num_outputs = action_space.n
        self.dist = CategoricalAction(self.base.hidden_layer_size[-1], num_outputs)

    @property
    def is_recurrent(self):
        return False

    @property
    def recurrent_hidden_state_size(self):
        return 1

    def forward(self, inputs, rnn_hxs, masks):
        raise NotImplementedError

    def act(self, inputs, rnn_hxs, masks, deterministic=False):
        value, actor_features = self.base(inputs)
        #TODO: Add permitted actions
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
