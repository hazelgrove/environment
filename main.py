import os
import sys
import time
from pathlib import Path
from typing import Optional
from unittest.result import failfast

import ray
import yaml
from git.repo import Repo
from ray import tune
from run_logger import RunLogger, create_sweep

from agent.arguments import get_args
from logger import get_charts, get_metadata
from trainer import GNNTrainer


def _log(
    name: str,
    repo: Repo,
    graphql_endpoint: str,
    sweep_id: Optional[int],
    save_dir: str,
    **kwargs
):
    logger = RunLogger(graphql_endpoint)
    logger.create_run(
        metadata=get_metadata(repo),
        sweep_id=sweep_id,
        charts=get_charts(),
    )
    logger.update_metadata(
        {
            "parameters": kwargs,
            "run_id": logger.run_id,
            "name": name,
        }
    )

    GNNTrainer.train(
        logger=logger, params=kwargs, log_name=name, render=False, save_dir=save_dir
    )


def trainable(config: dict):
    return _log(**config)


def sweep(
    name: str,
    config_path: str,
    graphql_endpoint: str,
    save_dir: str,
    random_search: bool = False,
):
    with open("params.yaml", "r") as file:
        params = yaml.safe_load(file)

    sweep_id = create_sweep(
        config=params,
        graphql_endpoint=graphql_endpoint,
        log_level="INFO",
        name=name,
    )

    config = {
        "name": name,
        "repo": Repo("."),
        "graphql_endpoint": graphql_endpoint,
        "save_dir": save_dir,
        "sweep_id": sweep_id,
        **{
            k: (tune.choice(v) if random_search else tune.grid_search(v))
            if isinstance(v, list)
            else v
            for k, v in params.items()
        },
    }

    runtime_env = {
        "working_dir": ".",
    }

    ray.init(runtime_env=runtime_env)
    num_cpus = os.cpu_count()
    analysis = ray.tune.run(
        trainable,
        config=config,
        resources_per_trial={"cpu": num_cpus, "gpu": 1},
        fail_fast="raise",
    )
    print(analysis.stats())


if __name__ == "__main__":
    args = get_args()

    sweep(
        args.log_name,
        config_path="params.yaml",
        graphql_endpoint=os.getenv("GRAPHQL_ENDPOINT"),
        save_dir=args.save_dir,
    )
