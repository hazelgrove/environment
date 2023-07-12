import argparse


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


if __name__ == "__main__":
    args = get_args()
    print(args)