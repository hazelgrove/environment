from torch_geometric.datasets import Planetoid
import torch
import torch.nn.functional as F
from torch_geometric.nn import GATv2Conv
import numpy as np
import random

from tqdm import tqdm

seed = 0
# Set the seed for PyTorch
torch.manual_seed(seed)

# If you are using CUDA (GPU), you also need to set the seed for the CUDA device
# This ensures reproducibility for GPU calculations as well
if torch.cuda.is_available():
    torch.cuda.manual_seed_all(seed)

# Set the seed for NumPy
np.random.seed(seed)

# Set the seed for Python's random module
random.seed(seed)

torch.use_deterministic_algorithms(True, warn_only=True)


class GCN(torch.nn.Module):
    def __init__(self):
        super().__init__()
        self.conv1 = GATv2Conv(dataset.num_node_features, 16)
        self.conv2 = GATv2Conv(16, dataset.num_classes)

    def forward(self, data):
        x, edge_index = data.x, data.edge_index

        x = self.conv1(x, edge_index)
        x = F.relu(x)
        x = F.dropout(x, training=self.training)
        x = self.conv2(x, edge_index)

        return F.log_softmax(x, dim=1)


dataset = Planetoid(root="/tmp/Cora", name="Cora")

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = GCN().to(device)
data = dataset[0].to(device)
optimizer = torch.optim.Adam(model.parameters(), lr=0.01, weight_decay=5e-4)

model.train()
optimizer.zero_grad()
for _ in tqdm(range(100)):
    out = model(data)
    loss = F.nll_loss(out[data.train_mask], data.y[data.train_mask])
    loss.backward()
    optimizer.step()

print(loss)