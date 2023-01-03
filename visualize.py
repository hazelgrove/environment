import os

import torch
import yaml
from run_logger import RunLogger, get_load_params

from agent.arguments import get_args_visualizer
from agent.envs import PLEnv
from agent.policy import GNNPolicy


def main(log_name, run_id):
    logger = RunLogger(os.getenv("GRAPHQL_ENDPOINT"))
    params = get_load_params(run_id, logger)
    # Account for changes in logging
    # params["env"]["cursor_start_pos"] = [6, 6]
    # params["env"]["perturbation"] = 0
    # params["env"]["max_episode_steps"] = 2
    # params["env"]["assignment_dir"] = "data/tests"
    
    params["base"]["num_assignments"] = params["env"]["num_assignments"]


    path = os.path.join("save", log_name, str(run_id) + ".pt")

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
        params["seed"], params["num_processes"], device, render=True, **env_kwargs
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
    env.render()
    while True:
        with torch.no_grad():
            (_, action, _, _,) = actor_critic.act(
                obs,
                None,
                None,
            )
        print(f"Action: {action}")
        breakpoint()
        obs, reward, done, info = env.step(action.reshape((-1,)))

        if done[0]:
            print(f"Reward: {info[0]['episode']['r']}")
            print()

            if info[0]["episode"]["r"] == 0:
                breakpoint()

            print("---------------Environment reset---------------")

        env.render()
        print()


if __name__ == "__main__":
    args = get_args_visualizer()

    main(args.log_name, args.run_id)
