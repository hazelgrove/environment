import torch.nn as nn

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


def update_linear_schedule(optimizer, epoch, total_num_epochs, initial_lr,targ_lr=None):
    """Decreases the learning rate linearly"""
    if targ_lr is None: 
        targ_lr = initial_lr/10
    diff = initial_lr -targ_lr
    lr = initial_lr - (diff * (epoch / float(total_num_epochs)))
    # cap lr to be > 0 
    lr = max(targ_lr,lr)
    for param_group in optimizer.param_groups:
        param_group["lr"] = lr

def update_entropy_schedule( epoch, total_num_epochs,targ_ent,initial_ent=None):
    """Decreases the learning rate linearly"""
    if initial_ent is None: 
        initial_ent = targ_ent*5 
    diff = initial_ent - targ_ent 
    ent = initial_ent - (diff * (epoch / float(total_num_epochs)))
    # dont undershoot by accident 
    ent = max(targ_ent,ent)
    return ent


def init(module, weight_init, bias_init, gain=1):
    weight_init(module.weight.data, gain=gain)
    bias_init(module.bias.data)
    return module
