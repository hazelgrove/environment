from typing import List, Optional

import numpy as np
import torch
import torch.nn as nn
import torch_geometric.nn as gnn

from agent.batch import collate, separate
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
        num_node_descriptor: int = 58,
        num_edge_descriptor: int = 6,
        num_assignments: int = 2,
        embedding_dim: int = 512,
        assignment_aggr: Optional[str] = None,
        max_num_vars: int = 11,
        device: Optional[torch.device] = None,
    ):
        super(GNNBase, self).__init__(False, 1, hidden_size)

        self.assignment_aggr = assignment_aggr

        self.gnn_layer_size = gnn_layer_size
        self.heads = heads
        self.hidden_size = hidden_size
        self.embedding_dim = embedding_dim
        self.max_num_vars = max_num_vars
        self.device = device

        # Check lengths for GNN hyperparameters
        # if len(gnn_layer_size) != 4:
        #     raise ValueError("GNN layer size must be a list of length 5")
        # if len(heads) != 5:
        #     raise ValueError("Heads must be a list of length 6")
        
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
                        directed_msg=True,
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
                        directed_msg=True,
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
                        directed_msg=True,
                    ),
                    "x, edge_index, edge_feature -> x",
                ),
                nn.ELU(),
                nn.Dropout(p=0.4),
                (
                    gnn.GeneralConv(
                        self.gnn_layer_size[2],
                        self.gnn_layer_size[3],
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[3],
                        directed_msg=True,
                    ),
                    "x, edge_index, edge_feature -> x",
                ),
                nn.ELU(),
                nn.Dropout(p=0.4),
                (
                    gnn.GeneralConv(
                        self.gnn_layer_size[3],
                        self.gnn_layer_size[4],
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[4],
                        directed_msg=True,
                    ),
                    "x, edge_index, edge_feature -> x",
                ),
                nn.ELU(),
                (
                    gnn.GeneralConv(
                        self.gnn_layer_size[4],
                        self.hidden_size,
                        in_edge_channels=self.embedding_dim,
                        attention=True,
                        heads=self.heads[5],
                        directed_msg=True,
                    ),
                    "x, edge_index -> x",
                ),
            ],
        )

        self.node_embedding = nn.Embedding(
            num_embeddings=num_node_descriptor + max_num_vars * 2 + 1,
            embedding_dim=embedding_dim,
            padding_idx=-1,
        )
        self.edge_embedding = nn.Embedding(
            num_embeddings=(num_edge_descriptor + 1) * 2,
            embedding_dim=embedding_dim,
        )
        self.assignment_embedding = nn.Embedding(num_assignments, embedding_dim)

        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )
        self.critic_linear = init_(nn.Linear(self.hidden_size, 1))

        self.train()

    def forward(self, inputs: torch.Tensor):
        batch_size = inputs["nodes"].shape[0]

        # Get corresponding data from inputs
        x = inputs["nodes"]
        edges = inputs["edges"].reshape((batch_size, -1, 3))
        edge_index = edges[:, :, :2]
        edge_index = edge_index.transpose(-2, -1)
        edge_attr = edges[:, :, 2]
        starter = inputs["starter"]
        assignment = inputs["assignment"]

        x, edge_index, edge_attr = collate(x, edge_index, edge_attr)
        edge_index = torch.concat((edge_index, edge_index.flip(0)), dim=1)
        edge_attr = torch.concat((edge_attr, edge_attr + 6), dim=0)

        # Convert inputs to long
        x = x.long()
        edge_index = edge_index.long()
        edge_attr = edge_attr.long()
        assignment = assignment.long()
        inputs["cursor_position"] = inputs["cursor_position"].long()
        inputs["vars_in_scope"] = inputs["vars_in_scope"].long()

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
        if self.assignment_aggr is not None:
            breakpoint()
            if self.assignment_aggr == "add":
                x += assignment
                edge_attr += assignment
            elif self.assignment_aggr == "concat":
                x = torch.concat((x, assignment), dim=-1)
                edge_attr = torch.concat((edge_attr, assignment), dim=-1)
            else:
                raise NotImplementedError

        # Append information on whether node can be changed
        starter = starter.reshape((-1, 1))
        x = torch.concat((x, starter), dim=-1)

        # Pass through GNN
        x = self.main(x, edge_index, edge_attr)
        x = separate(x, batch_size)

        # Get node representation at cursor
        out = x[torch.arange(batch_size), inputs["cursor_position"].flatten()]

        # Get node representation at variables in scope
        vars = x[
            torch.arange(batch_size)
            .reshape(-1, 1)
            .expand(batch_size, self.max_num_vars),
            inputs["vars_in_scope"],
        ]
        num_vars = torch.count_nonzero(inputs["vars_in_scope"] + 1, dim=1)
        mask = torch.zeros(batch_size, self.max_num_vars, device=vars.device)
        mask[torch.arange(batch_size), num_vars] = 1
        mask = mask.cumsum(dim=1).reshape(batch_size, -1, 1)
        vars = vars * (1 - mask)

        return self.critic_linear(out), out, vars
