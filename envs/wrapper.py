import gym
import torch


class ObsWrapper(gym.ObservationWrapper):
    def __init__(self, env, device) -> None:
        super().__init__(env)
        self.env = env
        self.device = device

    def observation(self, observation):
        nodes = torch.Tensor(observation["nodes"], device=self.device)
        edges = torch.Tensor(observation["edges"], device=self.device)
        permitted_actions = torch.Tensor(
            observation["permitted_actions"], device=self.device
        )
        cursor_position = torch.Tensor(
            [observation["cursor_position"]], device=self.device
        )
        vars_in_scope = torch.Tensor(observation["vars_in_scope"], device=self.device)
        assignment = torch.Tensor([observation["assignment"]], device=self.device)

        x = torch.cat(
            (
                nodes,
                edges,
                permitted_actions,
                cursor_position,
                vars_in_scope,
                assignment,
            )
        )

        return x

    def unwrap(x):
        pass
