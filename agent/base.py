from typing import List

import ipdb
import numpy as np
import torch
import torch.nn as nn
import torch_geometric.nn as gnn
from torch_geometric.data import Batch, Data

from agent.utils import init


class Flatten(nn.Module):
    def forward(self, x):
        return x.view(x.size(0), -1)


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
        hidden_size: int = 32,
        gnn_layer_size: List[int] = [128, 64, 64],
        heads: List[int] = [8, 8, 16, 1],
        num_node_descriptor: int = 50,
        num_edge_descriptor: int = 5,
        num_assignments: int = 1,
        embedding_dim: int = 512,
        assignment_aggr: str = "add",
    ):
        super(GNNBase, self).__init__(False, 1, hidden_size)

        self.assignment_aggr = assignment_aggr

        self.gnn_layer_size = gnn_layer_size
        self.heads = heads
        self.hidden_size = hidden_size
        self.embedding_dim = embedding_dim

        self.main = gnn.Sequential(
            "x, edge_index, edge_feature",
            [
                (
                    gnn.GeneralConv(
                        -1,
                        self.gnn_layer_size[0],
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[0],
                    ),
                    "x, edge_index, edge_feature -> x",
                ),
                nn.ELU(),
                nn.Dropout(p=0.4),
                (
                    gnn.GeneralConv(
                        self.gnn_layer_size[0],
                        self.gnn_layer_size[1],
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[1],
                    ),
                    "x, edge_index, edge_feature -> x",
                ),
                nn.ELU(),
                nn.Dropout(p=0.4),
                (
                    gnn.GeneralConv(
                        self.gnn_layer_size[1],
                        self.gnn_layer_size[2],
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[2],
                    ),
                    "x, edge_index, edge_feature -> x",
                ),
                nn.ELU(),
                (
                    gnn.GeneralConv(
                        self.gnn_layer_size[2],
                        self.hidden_size,
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[3],
                    ),
                    "x, edge_index -> x",
                ),
            ],
        )

        self.node_embedding = nn.Embedding(num_node_descriptor, embedding_dim)
        self.edge_embedding = nn.Embedding(num_edge_descriptor, embedding_dim)
        self.assignment_embedding = nn.Embedding(num_assignments, embedding_dim)

        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )
        self.critic_linear = init_(nn.Linear(self.hidden_size, 1))

        self.train()

    def forward(self, inputs):
        batch_size = inputs["nodes"].shape[0]

        # Get corresponding data from inputs
        x = inputs["nodes"]
        edges = inputs["edges"].reshape((batch_size, -1, 3))
        edge_index = edges[:, :, :2]
        edge_attr = edges[:, :, 2]
        assignment = inputs["assignment"]

        # Find number of valid nodes
        num_nodes = x[0].shape[0] * np.ones(batch_size, dtype=np.int64)
        num_edges = edge_index[0].shape[0] * np.ones(batch_size, dtype=np.int64)
        for i in range(batch_size):
            for j in range(x[i].shape[0]):
                if x[i, j] == -1:
                    num_nodes[i] = j
                    break
            for j in range(edge_index[i].shape[0]):
                if edge_index[i, j, 0] == -1:
                    num_edges[i] = j
                    break
        edge_index = edge_index.transpose(1, 2)
        print(f"Num Nodes: {num_nodes}")

        # Convert inputs to long
        x = x.long()
        edge_index = edge_index.long()
        edge_attr = edge_attr.long()
        assignment = assignment.long()

        # Convert to embedding
        x = self.node_embedding(x + 1)
        edge_attr = self.edge_embedding(
            edge_attr + 1
        )  # Shift 1 to make the -1 edge type to 0
        assignment = self.assignment_embedding(assignment)
        assignment = assignment.reshape(
            (-1, 1, self.embedding_dim)
        )  # Reshape assignment for broadcasting

        # Append assignment index to node and edge embeddings
        if self.assignment_aggr == "add":
            x += assignment
            edge_attr += assignment
        elif self.assignment_aggr == "concat":
            x = torch.concat((x, assignment), dim=-1)
            edge_attr = torch.concat((edge_attr, assignment), dim=-1)
        else:
            raise NotImplementedError

        data_list = []
        for i in range(batch_size):
            data_list.append(
                Data(
                    x=x[i, : num_nodes[i]],
                    edge_index=edge_index[i, :, : num_edges[i]],
                    edge_attr=edge_attr[i, : num_edges[i], :],
                )
            )
        data = Batch.from_data_list(data_list)

        # Pass through GNN & get info on current node
        data.x = self.main(data.x, data.edge_index, data.edge_attr)

        # Separate into individual points
        out = torch.zeros((batch_size, self.hidden_size))
        data_list = data.to_data_list()
        for i in range(batch_size):
            out[i] = data_list[i].x[int(inputs["cursor_position"][i])]

        return self.critic_linear(out), out
