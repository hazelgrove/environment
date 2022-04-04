from gym.envs.registration import register

register(id="test-v0", entry_point="gym_env.envs:TestEnv")
register(id="ast-v0", entry_point="gym_env.envs:ASTEnv")
