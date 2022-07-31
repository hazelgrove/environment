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
        type=bool,
        default=True,
        help="Whether the training is for our AST env"
    )
    args = parser.parse_args()

    return args
