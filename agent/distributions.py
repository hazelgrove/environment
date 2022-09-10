import math

import torch
import torch.nn as nn
import torch.nn.functional as F

from agent.utils import AddBias, init

"""
Modify standard PyTorch distributions so they are compatible with this code.
"""

#
# Standardize distribution interfaces
#


# Categorical
class FixedCategorical(torch.distributions.Categorical):
    def sample(self):
        return super().sample().unsqueeze(-1)

    def log_probs(self, actions):
        return (
            super()
            .log_prob(actions.squeeze(-1))
            .view(actions.size(0), -1)
            .sum(-1)
            .unsqueeze(-1)
        )

    def mode(self):
        return self.probs.argmax(dim=-1, keepdim=True)


# Normal
class FixedNormal(torch.distributions.Normal):
    def log_probs(self, actions):
        return super().log_prob(actions).sum(-1, keepdim=True)

    def entropy(self):
        return super().entropy().sum(-1)

    def mode(self):
        return self.mean


# Bernoulli
class FixedBernoulli(torch.distributions.Bernoulli):
    def log_probs(self, actions):
        return super.log_prob(actions).view(actions.size(0), -1).sum(-1).unsqueeze(-1)

    def entropy(self):
        return super().entropy().sum(-1)

    def mode(self):
        return torch.gt(self.probs, 0.5).float()


class Categorical(nn.Module):
    def __init__(self, num_inputs, num_outputs):
        super(Categorical, self).__init__()

        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0), gain=0.01
        )

        self.linear = init_(nn.Linear(num_inputs, num_outputs))

    def forward(self, x):
        x = self.linear(x)

        return FixedCategorical(logits=x)


class MaskedCategorical(nn.Module):
    def __init__(self):
        super().__init__()

    def forward(self, x, mask):
        x = x * mask
        x = x + -1e5 * (x == 0)

        return FixedCategorical(logits=x)


class DiagGaussian(nn.Module):
    def __init__(self, num_inputs, num_outputs):
        super(DiagGaussian, self).__init__()

        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )

        self.fc_mean = init_(nn.Linear(num_inputs, num_outputs))
        self.logstd = AddBias(torch.zeros(num_outputs))

    def forward(self, x):
        action_mean = self.fc_mean(x)

        #  An ugly hack for my KFAC implementation.
        zeros = torch.zeros(action_mean.size())
        if x.is_cuda:
            zeros = zeros.cuda()

        action_logstd = self.logstd(zeros)
        return FixedNormal(action_mean, action_logstd.exp())


class Bernoulli(nn.Module):
    def __init__(self, num_inputs, num_outputs):
        super(Bernoulli, self).__init__()

        init_ = lambda m: init(
            m, nn.init.orthogonal_, lambda x: nn.init.constant_(x, 0)
        )

        self.linear = init_(nn.Linear(num_inputs, num_outputs))

    def forward(self, x):
        x = self.linear(x)
        return FixedBernoulli(logits=x)


class QKV(nn.Module):
    def __init__(self, num_fixed_actions, embedding_size, max_num_vars) -> None:
        super().__init__()

        self.k = torch.nn.Parameter(torch.empty((num_fixed_actions, embedding_size)))
        self.k_arg = torch.nn.Parameter(
            torch.empty((max_num_vars * max_num_vars, embedding_size))
        )

        self.max_num_vars = max_num_vars

        self.reset_parameters()

    def reset_parameters(self):
        torch.nn.init.orthogonal_(self.k)
        torch.nn.init.orthogonal_(self.k_arg)

    def forward(self, q: torch.Tensor, k_var: torch.Tensor, args):
        B, D = q.shape
        q = q.reshape((B, 1, D))

        k = self.k.expand((B, -1, -1))
        k_arg = self.k_arg.expand((B, -1, -1))

        args = args.long()
        num_args = torch.count_nonzero(args + 1, dim=1)[:, 0]
        args = args[:, :, 0] * self.max_num_vars + args[:, :, 1]
        k_arg = k_arg[torch.arange(B).reshape(-1, 1).expand(B, self.max_num_vars), args]
        mask = torch.zeros(B, self.max_num_vars, device=k_arg.device)
        mask[torch.arange(B), num_args] = 1
        mask = mask.cumsum(dim=1).reshape(B, -1, 1)
        k_arg = k_arg * (1 - mask)

        k = torch.concat((k, k_var, k_arg), dim=1)

        attn = torch.bmm(q, k.transpose(-2, -1)) / math.sqrt(D)
        attn = attn.transpose(-2, -1).reshape((B, -1))  # attn : B x (N + N_var)

        return attn
