import glob
import os

import gym
import numpy as np
import torch
import torch.nn as nn
from gym.spaces.utils import unflatten

from agent.envs import VecNormalize


# Get a render function
def get_render_func(venv):
    if hasattr(venv, "envs"):
        return venv.envs[0].render
    elif hasattr(venv, "venv"):
        return get_render_func(venv.venv)
    elif hasattr(venv, "env"):
        return get_render_func(venv.env)

    return None


def get_vec_normalize(venv):
    if isinstance(venv, VecNormalize):
        return venv
    elif hasattr(venv, "venv"):
        return get_vec_normalize(venv.venv)

    return None


# Necessary for my KFAC implementation.
class AddBias(nn.Module):
    def __init__(self, bias):
        super(AddBias, self).__init__()
        self._bias = nn.Parameter(bias.unsqueeze(1))

    def forward(self, x):
        if x.dim() == 2:
            bias = self._bias.t().view(1, -1)
        else:
            bias = self._bias.t().view(1, -1, 1, 1)

        return x + bias


def update_linear_schedule(optimizer, epoch, total_num_epochs, initial_lr):
    """Decreases the learning rate linearly"""
    lr = initial_lr - (initial_lr * (epoch / float(total_num_epochs)))
    for param_group in optimizer.param_groups:
        param_group["lr"] = lr


def init(module, weight_init, bias_init, gain=1):
    weight_init(module.weight.data, gain=gain)
    bias_init(module.bias.data)
    return module


def cleanup_log_dir(log_dir):
    try:
        os.makedirs(log_dir)
    except OSError:
        files = glob.glob(os.path.join(log_dir, "*.monitor.csv"))
        for f in files:
            os.remove(f)


def batch_unflatten(space: gym.Space, x: np.ndarray):
    unflattened_vecs = []
    for vec in x:
        vec = unflatten(space, vec)
        unflattened_vecs.append(vec)

    return unflattened_vecs

def unpad_edge(edge_index, edge_attr):
    b, max_num_edges = edge_attr.shape
    num_edges = torch.count_nonzero(edge_index + 1, dim=2)[:, 0]
    
    edge_index = edge_index.transpose(-2, -1)
    grid = (
        torch.arange(max_num_edges, device=edge_index.device)
        .repeat(b, 2, 1)
        .transpose(-2, -1)
    )
    mask = grid < num_edges.reshape(b, 1, 1)
    edge_index = edge_index[mask].reshape(-1, 2).transpose(-2, -1)

    grid = (
        torch.arange(max_num_edges, device=edge_attr.device)
        .repeat(b, 1)
    )
    mask = grid < num_edges.reshape(b, 1)
    edge_attr = edge_attr[mask]

    return edge_index, edge_attr
