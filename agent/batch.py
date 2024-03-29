import torch


def collate(x, edge_index, edge_attr):
    b, num_nodes = x.shape
    num_edges = torch.count_nonzero(edge_index + 1, dim=2)[:, 0]
    x = x.reshape(b * num_nodes)

    inc = torch.arange(0, b * num_nodes, num_nodes, device=edge_index.device).reshape(
        b, 1, 1
    )
    edge_index = (edge_index + inc).transpose(-2, -1)

    max_num_edges = edge_index.shape[1]

    grid = (
        torch.arange(max_num_edges, device=edge_index.device)
        .repeat(b, 2, 1)
        .transpose(-2, -1)
    )
    mask = grid < num_edges.reshape(b, 1, 1)
    edge_index = edge_index[mask].reshape(-1, 2).transpose(-2, -1)

    grid = torch.arange(max_num_edges, device=edge_attr.device).repeat(b, 1)
    mask = grid < num_edges.reshape(b, 1)
    edge_attr = edge_attr[mask]

    return x, edge_index, edge_attr


def separate(x, batch_size):
    _, num_features = x.shape
    x = x.reshape(batch_size, -1, num_features)

    return x
