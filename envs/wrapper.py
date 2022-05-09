import gym
import torch


class ObsWrapper(gym.ObservationWrapper):
    def __init__(self, env, device=None) -> None:
        super().__init__(env)
        self.env = env
        self.device = device
        
        self.num_nodes = 0
        self.num_edges = 0

    def observation(self, observation):
        self.num_nodes = observation["num_nodes"]
        self.num_edges = observation["num_edges"]
        
        nodes = torch.Tensor(observation["nodes"][:self.num_nodes], device=self.device)
        
        edges = torch.Tensor(observation["edges"][:self.num_edges], device=self.device)
        edges = edges.flatten()
        
        permitted_actions = torch.Tensor(
            observation["permitted_actions"], device=self.device
        )
        cursor_position = torch.Tensor(
            [observation["cursor"]], device=self.device
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
        vecs = torch.split(x, (
            self.num_nodes,
            self.num_edges * 3,
            self.env.num_actions,
            1,
            self.env.max_num_vars,
            1,
        ))
        
        edges = vecs[1].reshape((-1, 3))
        edge_type = edges[:, 2]
        edges = edges[:, :2]
        print(vecs[3].item())
        
        return {
            "nodes": vecs[0],
            "edges": edges,
            "edge-type": edge_type,
            "permitted_actions": vecs[2],
            "cursor_position": int(vecs[3].item()),
            "vars_in_scope": vecs[4],
            "assignment": int(vecs[5].item()),
        }
