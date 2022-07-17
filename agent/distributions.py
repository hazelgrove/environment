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


class CategoricalAction(Categorical):
    def __init__(self, num_inputs: int, num_outputs: int):
        super().__init__(num_inputs, num_outputs)

    def forward(self, x, mask):
        x = self.linear(x)
        x = x * mask
        x[x == 0] = -1e5

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
    def __init__(self, num_actions, embedding_size) -> None:
        super().__init__()
        
        self.k_fixed = torch.nn.Parameter(torch.randn((num_actions, embedding_size)))
        self.gaussian = DiagGaussian(embedding_size, embedding_size)
        
    def forward(self, actor_features : torch.Tensor, k_var : torch.Tensor, attn_mask):
        # Sample q from gaussian
        q = self.gaussian(actor_features)
        
        B, E = q.shape
        q = q.reshape((B, 1, E))
        q = q / math.sqrt(E)
        
        # Concatenate k_fixed and k_var
        k = torch.cat((self.k_fixed, k_var), dim=1)
        
        #TODO: transpose of q?
        attn = torch.baddbmm(attn_mask, q, k.transpose(-2, -1))
        attn = attn.transpose(-2, -1).reshape((B, -1))
        
        return attn
        
        
