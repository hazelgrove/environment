import gym
import torch
import numpy as np


class ObsWrapper(gym.ObservationWrapper):
    def __init__(self, env) -> None:
        super().__init__(env)
        
        self.sizes = []

    def observation(self, observation):
        arrs = []
        sizes = []
        curr_index = 0
        
        for value in observation.values():
            if isinstance(value, int):
                value = np.array([value])
            elif isinstance(value, list):
                value = np.array(value)
            elif not isinstance(value, np.ndarray):
                raise NotImplementedError
            
            value = value.flatten()
            arrs.append(value)
            
            curr_index += value.shape[0]
            sizes.append(curr_index)
        
        self.sizes = sizes
        return np.concatenate(arrs)

    def unwrap(self, x):
        # TODO: Not sure if we can do it in systematic way
        
        vecs = np.split(x, self.sizes)
        print(vecs)
        
        edges = vecs[1].reshape((-1, 3))
        edge_type = edges[:, 2]
        edges = edges[:, :2]
        
        return {
            "nodes": vecs[0],
            "edges": edges.transpose(),
            "edge-type": edge_type,
            "assignment": int(vecs[2][0]),
            "cursor": int(vecs[3][0]),
            "permitted_actions": vecs[4],
            "vars_in_scope": vecs[5],
        }
