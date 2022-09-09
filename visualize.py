import os

import torch
import yaml

from agent.envs import PLEnv
from agent.arguments import get_args
from agent.policy import GNNPolicy

def main(log_name):
    path = os.path.join('save', log_name, 'ASTEnv.pt')
    
    with open('params.yaml', 'r') as file:
        params = yaml.safe_load(file)
        
    torch.manual_seed(params["seed"])
    torch.cuda.manual_seed_all(params["seed"])

    if params["cuda"] and torch.cuda.is_available() and params["cuda_deterministic"]:
        torch.backends.cudnn.benchmark = False
        torch.backends.cudnn.deterministic = True
        
    torch.set_num_threads(1)
    device = torch.device("cuda:0" if params["cuda"] else "cpu")
    
    params["num_processes"] = 1
    env_kwargs = params["env"]
    env = PLEnv.make_vec_envs(
        params["seed"],
        params["num_processes"],
        device,
        render=True,
        **env_kwargs
    )
    
    base_kwargs = params["base"]
    actor_critic = GNNPolicy(
        env.get_attr("orig_obs_space")[0],
        env.get_attr("action_space")[0],
        env.get_attr("num_actions")[0],
        base_kwargs=base_kwargs,
        device=device,
    )
    actor_critic.to(device)
    actor_critic.load_state_dict(torch.load(path)[0])
    actor_critic.eval()
    
    obs = env.reset()
    while True:
        with torch.no_grad():
            (
                _,
                action,
                _,
                _,
            ) = actor_critic.act(
                obs,
                None,
                None,
            )
        print(f"Action: {action}")
        obs, reward, done, info = env.step(action.reshape((-1, )))
        
        if done[0]:
            print(f"Reward: {info[0]['episode']['r']}")
            print()
            print("---------------Environment reset---------------")
        
        env.render()
        print()
        breakpoint()
    

if __name__ == "__main__":
    args = get_args()
    
    main(args.log_name)