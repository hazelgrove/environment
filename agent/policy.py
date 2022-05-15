from typing import List
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch_geometric.nn as gnn

from agent.distributions import Bernoulli, Categorical, DiagGaussian
from agent.utils import init
import ipdb
from agent.utils import batch_unflatten


class Flatten(nn.Module):
    def forward(self, x):
        return x.view(x.size(0), -1)


class Policy(nn.Module):
    def __init__(self, obs_space, action_space, base=None, base_kwargs=None, dist_mask=False):
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
            
        if base == CNNBase or base == MLPBase:
            self.base = base(obs_space.shape[0], **base_kwargs)
        else:
            self.base = GNNBase(obs_space, **base_kwargs)

        if action_space.__class__.__name__ == "Discrete":
            num_outputs = action_space.n
            self.dist = Categorical(self.base.output_size, num_outputs, has_mask=dist_mask)
        elif action_space.__class__.__name__ == "Box":
            num_outputs = action_space.shape[0]
            self.dist = DiagGaussian(self.base.output_size, num_outputs)
        elif action_space.__class__.__name__ == "MultiBinary":
            num_outputs = action_space.shape[0]
            self.dist = Bernoulli(self.base.output_size, num_outputs)
        else:
            raise NotImplementedError
        self.dist_mask = dist_mask

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
        if self.dist_mask:
            #TODO: batch
            dist = self.dist(actor_features, self.env.unwrap(inputs)["permitted_actions"])
        else:
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


class NNBase(nn.Module):
    def __init__(self, recurrent, recurrent_input_size, hidden_size):
        super(NNBase, self).__init__()

        self._hidden_size = hidden_size
        self._recurrent = recurrent

        if recurrent:
            self.gru = nn.GRU(recurrent_input_size, hidden_size)
            for name, param in self.gru.named_parameters():
                if "bias" in name:
                    nn.init.constant_(param, 0)
                elif "weight" in name:
                    nn.init.orthogonal_(param)

    @property
    def is_recurrent(self):
        return self._recurrent

    @property
    def recurrent_hidden_state_size(self):
        if self._recurrent:
            return self._hidden_size
        return 1

    @property
    def output_size(self):
        return self._hidden_size

    def _forward_gru(self, x, hxs, masks):
        if x.size(0) == hxs.size(0):
            x, hxs = self.gru(x.unsqueeze(0), (hxs * masks).unsqueeze(0))
            x = x.squeeze(0)
            hxs = hxs.squeeze(0)
        else:
            # x is a (T, N, -1) tensor that has been flatten to (T * N, -1)
            N = hxs.size(0)
            T = int(x.size(0) / N)

            # unflatten
            x = x.view(T, N, x.size(1))

            # Same deal with masks
            masks = masks.view(T, N)

            # Let's figure out which steps in the sequence have a zero for any agent
            # We will always assume t=0 has a zero in it as that makes the logic cleaner
            has_zeros = (masks[1:] == 0.0).any(dim=-1).nonzero().squeeze().cpu()

            # +1 to correct the masks[1:]
            if has_zeros.dim() == 0:
                # Deal with scalar
                has_zeros = [has_zeros.item() + 1]
            else:
                has_zeros = (has_zeros + 1).numpy().tolist()

            # add t=0 and t=T to the list
            has_zeros = [0] + has_zeros + [T]

            hxs = hxs.unsqueeze(0)
            outputs = []
            for i in range(len(has_zeros) - 1):
                # We can now process steps that don't have any zeros in masks together!
                # This is much faster
                start_idx = has_zeros[i]
                end_idx = has_zeros[i + 1]

                rnn_scores, hxs = self.gru(
                    x[start_idx:end_idx], hxs * masks[start_idx].view(1, -1, 1)
                )

                outputs.append(rnn_scores)

            # assert len(outputs) == T
            # x is a (T, N, -1) tensor
            x = torch.cat(outputs, dim=0)
            # flatten
            x = x.view(T * N, -1)
            hxs = hxs.squeeze(0)

        return x, hxs


class CNNBase(NNBase):
    def __init__(self, num_inputs, recurrent=False, hidden_size=512):
        super(CNNBase, self).__init__(recurrent, hidden_size, hidden_size)

        init_ = lambda m: init(
            m,
            nn.init.orthogonal_,
            lambda x: nn.init.constant_(x, 0),
            nn.init.calculate_gain("relu"),
        )

        self.main = nn.Sequential(
            init_(nn.Conv2d(num_inputs, 32, 8, stride=4)),
            nn.ReLU(),
            init_(nn.Conv2d(32, 64, 4, stride=2)),
            nn.ReLU(),
            init_(nn.Conv2d(64, 32, 3, stride=1)),
            nn.ReLU(),
            Flatten(),
            init_(nn.Linear(32 * 7 * 7, hidden_size)),
            nn.ReLU(),
        )

        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )

        self.critic_linear = init_(nn.Linear(hidden_size, 1))

        self.train()

    def forward(self, inputs, rnn_hxs, masks):
        x = self.main(inputs / 255.0)

        if self.is_recurrent:
            x, rnn_hxs = self._forward_gru(x, rnn_hxs, masks)

        return self.critic_linear(x), x, rnn_hxs


class MLPBase(NNBase):
    def __init__(self, num_inputs, recurrent=False, hidden_size=64):
        super(MLPBase, self).__init__(recurrent, num_inputs, hidden_size)

        if recurrent:
            num_inputs = hidden_size

        init_ = lambda m: init(
            m,
            nn.init.orthogonal_,
            lambda x: nn.init.constant_(x, 0),
            np.sqrt(2),
        )

        self.actor = nn.Sequential(
            init_(nn.Linear(num_inputs, hidden_size)),
            nn.Tanh(),
            init_(nn.Linear(hidden_size, hidden_size)),
            nn.Tanh(),
        )

        self.critic = nn.Sequential(
            init_(nn.Linear(num_inputs, hidden_size)),
            nn.Tanh(),
            init_(nn.Linear(hidden_size, hidden_size)),
            nn.Tanh(),
        )

        self.critic_linear = init_(nn.Linear(hidden_size, 1))

        self.train()

    def forward(self, inputs, rnn_hxs, masks):
        x = inputs

        if self.is_recurrent:
            x, rnn_hxs = self._forward_gru(x, rnn_hxs, masks)

        hidden_critic = self.critic(x)
        hidden_actor = self.actor(x)

        return self.critic_linear(hidden_critic), hidden_actor, rnn_hxs
    
    
class GNNBase(NNBase):
    def __init__(
        self,
        obs_space,
        hidden_size=32,
        gnn_layer_size: List[int] = [128, 64],
        heads: List[int] = [8, 8, 16, 1],
        in_edge_channels: int = 10,
        num_node_descriptor: int = 50,
        num_edge_descriptor: int = 4,
        num_assignments: int = 1,
        embedding_dim: int = 512,
        assignment_aggr: str = "add",
    ):
        super(GNNBase, self).__init__(False, 1, hidden_size)
        
        self.obs_space = obs_space
        self.assignment_aggr = assignment_aggr
        
        self.gnn_layer_size = gnn_layer_size
        self.heads = heads
        self.in_edge_channels = in_edge_channels
        self.hidden_size = hidden_size
        
        self.main = gnn.Sequential('x, edge_index, edge_feature', [
            (gnn.GeneralConv(
                -1, 
                self.hidden_layer_size[0],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[0],
            ), 'x, edge_index, edge_feature -> x'),
            nn.ELU(),
            nn.Dropout(p=0.4),
            (gnn.GeneralConv(
                self.hidden_layer_size[0], 
                self.hidden_layer_size[1],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[1],
            ), 'x, edge_index, edge_feature -> x'),
            nn.ELU(),
            nn.Dropout(p=0.4),
            (gnn.GeneralConv(
                self.hidden_layer_size[1], 
                self.hidden_layer_size[2],
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[2],
            ), 'x, edge_index, edge_feature -> x'),
            nn.ELU(),
            (gnn.GeneralConv(
                self.hidden_layer_size[2],
                self.hidden_size,
                in_edge_channels=self.in_edge_channels,
                attention=True,
                heads=self.heads[3],
            ), 'x, edge_index -> x')
        ])
        
        self.node_embedding = nn.Embedding(num_node_descriptor, embedding_dim)
        self.edge_embedding = nn.Embedding(num_edge_descriptor, embedding_dim)
        self.assignment_embedding = nn.Embedding(num_assignments, embedding_dim)
        
        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )
        self.critic_linear = init_(nn.Linear(self.hidden_size, 1))
        
        self.train()

    def forward(self, data, rnn_hxs, masks):
        # Unwrap the given obs array
        data = batch_unflatten(self.obs_space, data)
        
        # Reshape nodes so that each row is one node
        x = data["nodes"].reshape((-1, 1))
        
        # Reshape and split edges into adjacent nodes' indices and edge attribute
        edge = data["edges"]
        edge_index = edge[:, :2].reshape((2, -1))
        edge_attr = edge[:, 2].reshape((-1, 1))
        
        assignment = data["assignment"]
        
        # Change descriptor numbering to embedding
        x = x.long()
        x = self.node_embedding(x)
        edge_attr = edge_attr.long()
        edge_attr = self.edge_embedding(edge_attr)
        assignment = assignment.long()
        assignment = self.assignment_embedding(assignment)
        
        # Append assignment index to node and edge embeddings
        if self.assignment_aggr == "add":
            x += assignment
            edge_attr += assignment
        elif self.assignment_aggr == "concat":
            x = torch.concat((x, assignment), dim=-1)
            edge_attr = torch.concat((edge_attr, assignment), dim=-1)
        else:
            raise NotImplementedError
        
        # Pass through GNN & get info on current node
        x = self.main(x, edge_index, edge_attr)
        
        ipdb.set_trace()
        x = x[data["cursor"]]
        
        return self.critic_linear(x), x, rnn_hxs
