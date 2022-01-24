import gym
import sys

sys.path.append('../gym_basic')
import gym_basic.spaces.ASTSpace

class ASTEnv(gym.Env):
    def __init__(self, num_actions):
        super(ASTEnv, self).__init__()

        self.action_space = gym.spaces.Discrete(num_actions)
        self.observation_space = None  # TODO: Need to specify

    # TODO: Connect to OCaml and adjust & evaluate AST
    def step(self, action):
        pass

    # TODO: Reset to original AST
    def reset(self):
        pass

    # TODO: Put a visual?
    def render(self, mode="human"):
        pass

    # TODO: Anything that needs to be cleaned up
    def close(self):
        pass
