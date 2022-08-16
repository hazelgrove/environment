import torch

def collate(x, edge_index, edge_attr):
    b, num_nodes, num_features = x.shape
    num_edges = torch.count_nonzero(edge_index + 1, dim=2)[:, 0]
    x = x.reshape(b * num_nodes, num_features)
    
    inc = torch.arange(0, b * num_nodes, num_nodes, device=edge_index.device).reshape(b, 1, 1)
    edge_index = edge_index + inc
    
    edge_index = torch.concat([
        edge_index[i, :, :num_edges[i]] for i in range(b)
    ], dim=1)
    edge_attr = torch.concat([
        edge_attr[i, :num_edges[i], :] for i in range(b)
    ], dim=0)
    
    return x, edge_index, edge_attr

def separate(x, batch_size):
    num_nodes, num_features = x.shape
    x = x.reshape(batch_size, -1, num_features)
    
    return x