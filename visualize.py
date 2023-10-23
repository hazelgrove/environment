import os

import torch
import yaml

from agent.arguments import get_args_visualizer
from test_gen_util import generate_tests
from agent.envs import PLEnv
from agent.policy import GNNPolicy


def main(save_dir, log_name, run_id):
    print(f"Visualizing {log_name}...")
    
    # Get save directory
    save_dir = os.path.join(save_dir, log_name)
    with open(os.path.join(save_dir, "params.yaml"), 'r') as f:
        params = yaml.safe_load(f)

    torch.manual_seed(params["seed"])
    torch.cuda.manual_seed_all(params["seed"])

    if params["cuda"] and torch.cuda.is_available() and params["cuda_deterministic"]:
        torch.backends.cudnn.benchmark = False
        torch.backends.cudnn.deterministic = True

    torch.set_num_threads(1)
    device = torch.device("cuda" if params["cuda"] else "cpu")

    params["num_processes"] = 1
    env_kwargs = params["env"] # used to be "eval"
    env_kwargs["max_episode_steps"] = params["env"]["max_episode_steps"]
    env = PLEnv.make_vec_envs(
        params["seed"], params["num_processes"], device, render=True, **env_kwargs
    )

    base_kwargs = params["base"]
    actor_critic = GNNPolicy(
        env.get_attr("orig_obs_space")[0],
        env.get_attr("action_space")[0],
        env.get_attr("num_actions")[0],
        base_kwargs=base_kwargs,
        device=device,
        done_action = params['env']['done_action'],
    )
    actor_critic.to(device)
    actor_critic.load_state_dict(torch.load(os.path.join(save_dir, 'params.pt'))[0])
    actor_critic.eval()

    obs = env.reset()
    env.render()
    step = 0 
    stop_on_update = False 
    made_hole = False
    stop_on_hole=False
    stop_on_fail=True 
    stop_on_hole_success=True
    stop_on_new_episode=True
    num_tests = 0
    while True:
        with torch.no_grad():
            (_, action, _, _,) = actor_critic.act(
                obs,
                None,
                None,
            )
        if stop_on_new_episode and step == 0: breakpoint()
        step +=1
        print(f'step: {step}')
        print(f"Action: {action}")
        if stop_on_update: 
            breakpoint() # to allow manual change, action[0] = n 
        print()
        # if needed add python input l
        if action[0] == 7: 
            made_hole=True
            if stop_on_hole: breakpoint()
        
        obs, reward, done, info = env.step(action.reshape((-1,)))

        if done[0]:
            print(f"Reward: {info[0]['episode']['r']}")
            step = 0 
            print()
            print(f'step: {step}')
            # breakpoint()
            if info[0]["episode"]["r"] == 0:
                print('failed')
                if stop_on_fail: breakpoint()
            else: 
                # succeeded 
                if made_hole: 
                    print('Succeeded despite hole creation')
                    if stop_on_hole_success: breakpoint()

        
            num_tests +=1
            # breakpoint()
            print("---------------Environment reset---------------")
            if num_tests % 5000 == 0 : 
                breakpoint()

            made_hole=False
        env.render()
        print()


if __name__ == "__main__":
    args = get_args_visualizer()

    main(args.save_dir, args.log_name, args.run_id)
