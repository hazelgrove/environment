import os
from pathlib import Path
import sys
import time

import numpy as np
from run_logger import initialize
from agent import base
from git import Repo


def get_charts():
    update_reward = {
        "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
        "description": "Mean Episode Reward Over Update",
        "data": {"data": "data", "values": []},
        "transform": [{}],
        "mark": "line",
        "encoding": {
            "x": {"field": "update", "type": "quantitative"},
            "y": {"field": "mean_episode_rewards", "type": "quantitative"},
        },
    }

    return [
        update_reward,
    ]
    

def get_metadata(name, env_kwargs, base_kwargs, ppo_kwargs):
    repo = Repo.init(os.getcwd())
    
    return dict(
        name=name,
        env=env_kwargs,
        base=base_kwargs,
        ppo=ppo_kwargs,
        reproducibility=dict(
            command_line=f'python {" ".join(sys.argv)}',
            time=time.strftime("%c"),
            cwd=str(Path.cwd()),
            commit=str(repo.commit()),
            remotes=[*repo.remote().urls],
        ),
    )


def get_logger(name, env_kwargs, base_kwargs, ppo_kwargs):
    config = "logger_config.yaml"
    charts = get_charts()

    params, logger = initialize(
        graphql_endpoint=os.getenv("GRAPHQL_ENDPOINT"),
        config=config,
        charts=charts,
        load_id=None,
        metadata=get_metadata(name, env_kwargs, base_kwargs, ppo_kwargs),
    )

    return params, logger
