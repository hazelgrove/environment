import torch
import torch.nn as nn
import torch_geometric.nn as gnn
from torch.autograd import Variable

class GatedGNN(gnn.conv.MessagePassing):
    def __init__(
        self,
        out_channels: int,
        num_layers: int,
        aggr: str = 'add',
        bias: bool = True,
        **kwargs
    ):
        super().__init__(aggr=aggr, **kwargs)
        
        self.out_channels = out_channels
        self.num_layers = num_layers
        
        self.weight = nn.Parameter(torch.Tensor(num_layers, out_channels, out_channels))
        self.rnn = nn.GRUCell(out_channels, out_channels, bias=bias)
        
        self.lin_edge = nn.Linear(out_channels, out_channels)
        
        self.reset_parameters()
        
    def reset_parameters(self):
        nn.init.uniform_(self.weight)
        self.rnn.reset_parameters()
        self.lin_edge.reset_parameters()
        
    def forward(self, x: torch.Tensor, edge_index: torch.Tensor, edge_attr = None) -> torch.Tensor:
        if x.size(-1) > self.out_channels:
            raise ValueError('The number of input channels is not allowed to '
                             'be larger than the number of output channels')

        if x.size(-1) < self.out_channels:
            zero = x.new_zeros(x.size(0), self.out_channels - x.size(-1))
            x = torch.cat([x, zero], dim=1)
        if edge_attr.size(-1) < self.out_channels:
            zero = edge_attr.new_zeros(edge_attr.size(0), self.out_channels - edge_attr.size(-1))
            edge_attr = torch.cat([edge_attr, zero], dim=1)

        for i in range(self.num_layers):
            m = torch.matmul(x, self.weight[i])
            m = self.propagate(edge_index, x=m, edge_attr=edge_attr)
            x = self.rnn(m, x)
        
        return x

    def message(self, x_j: torch.Tensor, edge_attr = None) -> torch.Tensor:
        if edge_attr is None:
            return x_j
        else:
            return x_j + self.lin_edge(edge_attr)
        

class CursorRNN(nn.Module):
    def __init__(
        self,
        input_dim: int,
        hidden_dim: int,
        output_dim: int,
        num_layers: int,
        **kwargs
    ):
        super().__init__(**kwargs)
        
        self.input_dim = input_dim
        self.hidden_dim = hidden_dim
        self.output_dim = output_dim
        self.num_layers = num_layers
        
        self.rnn = nn.RNN(input_dim, hidden_dim, num_layers=num_layers, batch_first=True)
        
        self.fc = nn.Linear(hidden_dim, output_dim)
        
        self.reset_parameters()
        
    def reset_parameters(self):
        self.rnn.reset_parameters()
        self.fc.reset_parameters()
        
    def forward(self, x):
        out, _ = self.rnn(x)
        out = self.fc(out[:, -1, :]) 
        
        return out