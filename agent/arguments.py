import argparse
import yaml


def get_args():
    parser = argparse.ArgumentParser(description="RL")
    parser.add_argument("--log-name", default="None", help="Name for the log")
    parser.add_argument("--sweep", action="store_true", help="Whether to use sweeps")
    parser.add_argument("--render", action="store_true", help="Whether to render the environment")
    parser.add_argument(
        "--save-dir", default="None", help="Directory to save checkpoints"
    )
    parser.add_argument(
        "--sweep-id",
        default="None",
        help="the ID of the sweep, appended to the end of the save dir",
    )
    # resume args 
    parser.add_argument('-r','--resume',action='store_true',default=False,help="Resume from a previous run & run ID. Must be used with --resume_id and --resume_name Copies over base and env parameters from resumed run, and takes lr, and general parameters from params.yaml")
    parser.add_argument('--resume_id',default=None,type=int,help='Specify the id of the run to resume from. used with --resume and resume_name to resume from a previous run ')
    parser.add_argument('--resume_name',default=None,type=str,help='Specify the name of the run to resume from. used with --resume and resume_id to resume from a previous run ')

    args = parser.parse_args()


    return args


def get_args_visualizer():
    parser = argparse.ArgumentParser(description="Visualizer")
    parser.add_argument(
        "--run-id", default="None", help="Run ID for the log to visualize"
    )
    parser.add_argument("--log-name", default="None", help="Name for the log")
    args = parser.parse_args()

    return args

# read config file
def read_params(config_path:str): 
    with open(config_path, "r") as file:
        params = yaml.safe_load(file)
    
    # can speicify to load auto-generated config file for train and test envs
    for name in ['env','eval']: 
        if name in params.keys() :
            get_froms = [key for key in params[name].keys() if 'get_from' in key]
            loaded_params = []
            for get_from in get_froms:
                print(f'getting from {params[name][get_from]}')
                params_file =params[name][get_from]
                with open(params_file,'r') as env_file: 
                    env_params = yaml.safe_load(env_file)
                args = env_params[name] # allows us to define both in same file
                loaded_params.append(args)
                print(f'fetched {name} params from file {params_file}')
                print(env_params[name])
                params[name].pop(get_from) 

            # enforce certain requirements: all datasets need to have same 'done action' param
            pull_params = ['done_action','perturbation']
            for pull_param in pull_params:
                if pull_param not in params[name] and pull_param in loaded_params[0]: 
                    targ_done_action = loaded_params[0][pull_param]
                    for i in range(len(loaded_params)):
                        loaded_params[i][pull_param] = targ_done_action
                    params[name][pull_param] = targ_done_action

            # also, pull out max episode steps val, as its used elsewhere 
            if 'max_episode_steps' not in params[name]:
                max_steps = max( paramset['max_episode_steps'] for paramset in loaded_params )
                params[name]['max_episode_steps'] = max_steps
            if len(loaded_params) == 1: 
                params[name] = loaded_params[0]
            elif len(loaded_params) >1: 
                for dsnum, dsparams in enumerate(loaded_params):
                    # cleaned = str.replace(dsname,'get_from','_')
                    dsname = f'dataset_{dsnum}'
                    params[name][dsname] = dsparams
                    
    return params


if __name__ == "__main__":
    params = read_params('params.yaml')
    print(yaml.dump(params))