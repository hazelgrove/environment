import argparse


def get_args():
    parser = argparse.ArgumentParser(description="RL")
    parser.add_argument("--log-name", default="None", help="Name for the log")
    parser.add_argument("--sweep", action="store_true", help="Whether to use sweeps")
    parser.add_argument(
        "--render", action="store_true", help="Whether to render the environment"
    )
    parser.add_argument(
        "--save-dir", default="None", help="Directory to save checkpoints"
    )
    parser.add_argument(
        "--sweep-id",
        default="None",
        help="the ID of the sweep, appended to the end of the save dir",
    )
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
