from envs.ast_env import ASTEnv
from envs.wrapper import ObsWrapper
import torch


def main():
    env = ASTEnv(
        max_num_nodes=20,
        num_node_descriptor=50,
        num_assignments=1,
        code_per_assignment=[1],
        num_actions=80,
    )
    wrapped_env = ObsWrapper(env)
    torch.set_printoptions(threshold=10000)
    
    obs = env.reset()
    print(obs)
    
    wrapped_obs = wrapped_env.observation(obs)
    orig_obs = wrapped_env.unwrap(wrapped_obs)
    print(wrapped_obs)
    print(orig_obs)
    


if __name__ == "__main__":
    main()