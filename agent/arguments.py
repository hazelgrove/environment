import argparse

import torch


def get_args():
    parser = argparse.ArgumentParser(description="RL")
    parser.add_argument(
        "--log-name",
        default="None",
        help="Name for the log"
    )
    parser.add_argument(
        "--gnn",
        action="store_true",
        help="Whether the training is for our AST env"
    )
    parser.add_argument(
        "--render",
        action="store_true",
        help="Whether to render the environment"
    )
    parser.add_argument(
        "--save-dir",
        default="None",
        help="Directory to save checkpoints"
    )
    args = parser.parse_args()

    return args
