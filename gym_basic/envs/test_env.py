import gym


class TestEnv(gym.Env):
    def __init__(self):
        super(TestEnv, self).__init__()

        # Define action space
        self.action_space = gym.spaces.Discrete(5)

        # Define States
        self.observation_space = gym.spaces.Discrete(2)

    def step(self, action):
        state = 1
        if action == 0:
            reward = 1
        else:
            reward = -1
        done = True
        info = {}

        return state, reward, done, info

    def reset(self):
        state = 0
        return state

