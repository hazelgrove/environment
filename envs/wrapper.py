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
        edges = edges.flatten(edges)
        
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

    def unwrap(self, x):
        vecs = torch.split(x, [
            self.env.node_nvec,
            self.env.edge_nvec * 3,
            self.env.num_actions,
            1,
            self.env.max_num_vars,
            1,
        ])
        
        edges = vecs[1].reshape((-1, 3))
        edge_type = torch.flattten(edges[:, 2])
        edges = edges[:, :2]
        
        return {
            "nodes": vecs[0],
            "edges": edges,
            "edge-type": edge_type,
            "permitted_actions": vecs[2],
            "cursor_position": vecs[3][0],
            "vars_in_scope": vecs[4],
            "assignment": vecs[5]
        }
