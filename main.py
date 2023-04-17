import os
import sys
import time
from pathlib import Path
from typing import Optional
from unittest.result import failfast

import ray
import torch
import yaml
from git.repo import Repo
from ray import tune
from run_logger import RunLogger, create_sweep

from agent.arguments import get_args
from logger import get_charts, get_metadata
from trainer import GNNTrainer

from ray.air.config import RunConfig, ScalingConfig
from ray.air.integrations.wandb import WandbLoggerCallback


def _log(
    name: str,
    repo: Repo,
    graphql_endpoint: str,
    save_dir: str,
    sweep_id: Optional[int] = None,
    render: bool = False,
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
        logger=logger, params=kwargs, log_name=name, render=render, save_dir=save_dir
    )


def run(
    name: str,
    config_path: str,
    graphql_endpoint: str,
    save_dir: str,
    render: bool = False,
):
    with open(config_path, "r") as file:
        params = yaml.safe_load(file)

    _log(
        name=name,
        repo=Repo("."),
        graphql_endpoint=graphql_endpoint,
        save_dir=save_dir,
        sweep_id=None,
        render=render,
        **params,
    )


def trainable(config: dict):
    return _log(**config)


def sweep(
    name: str,
    config_path: str,
    graphql_endpoint: str,
    save_dir: str,
    random_search: bool = False,
    render: bool = False,
):
    with open(config_path, "r") as file:
        params = yaml.safe_load(file)

    sweep_id = create_sweep(
        config=params,
        graphql_endpoint=graphql_endpoint,
        log_level="INFO",
        name=name,
    )
    
    hyperparam_names = [
        "base",
        "ppo",
        "return",
    ]
    for section in params.keys():
        if section in hyperparam_names:
            params[section] = {
                k: (tune.choice(v) if random_search else tune.grid_search(v))
                if isinstance(v, list)
                else v
                for k, v in params[section].items()
            }
    num_cpus = os.cpu_count()
    num_gpus = torch.cuda.device_count()

    config = {
        "name": name,
        "repo": Repo("."),
        "graphql_endpoint": graphql_endpoint,
        "save_dir": save_dir,
        "sweep_id": sweep_id,
        "wandb": {"project": name},
        **params,
    }

    ray.init()
    tuner = ray.tune.Tuner(
        tune.with_resources(
            tune.with_parameters(trainable),
            resources={"cpu": int(num_cpus / num_gpus), "gpu": 1}
        ),
        param_space=config,
        tune_config=tune.TuneConfig(num_samples=10),
        # run_config=RunConfig(callbacks=[WandbLoggerCallback(project=name, api_key_file="/RL_env/wandb_api_key")]),
    )

    results = tuner.fit()
    print(results)


if __name__ == "__main__":
    args = get_args()
    print("Python started.")

    if args.sweep:
        sweep(
            name=args.log_name,
            config_path="params.yaml",
            graphql_endpoint=os.getenv("GRAPHQL_ENDPOINT"),
            save_dir=args.save_dir,
            render=args.render,
        )
    else:
        run(
            name=args.log_name,
            config_path="params.yaml",
            graphql_endpoint=os.getenv("GRAPHQL_ENDPOINT"),
            save_dir=args.save_dir,
            render=args.render,
        )
