import time
import numpy as np
import matplotlib.pyplot as plt

# from envs.ast_env import ASTEnv

def main(num_iter = 10000):
    env = ASTEnv(
                max_num_nodes=250,
                num_node_descriptor=107,
                num_assignments=1,
                code_per_assignment=[1],
                num_actions=132,
                perturbation=0,
                seed=0,
                assignment_dir='data/profile/4-var',
                cursor_start_pos=[0],
                done_action=True,
            )
    
    obs = env.reset()
    
    start = time.time()
    for _ in range(num_iter):
        permitted_actions = np.nonzero(obs['permitted_actions'])[0]
        action = np.random.choice(permitted_actions)
        
        _ = env.step(action)
        obs = env.reset()
    end = time.time()
    
    return end - start


def plot():
    num_vars = np.arange(4)
    
    fps = [
        888.4973250098,
        770.080660745211,
        544.5392231190701,
        280.65078829621154,
    ]
    
    num_nodes = [
        11,
        25,
        56,
        122,
    ]
    
    plt.plot(num_nodes, fps, 'o-')
    plt.xlabel('Number of Nodes')
    plt.ylabel('Frames per Second (FPS)')
    plt.title('FPS vs. Number of Nodes for 1-Timestep Environment')
    plt.savefig('profile.png')


if __name__ == "__main__":
    # num_iter = 10000
    # print(num_iter / main(num_iter))
    
    plot()