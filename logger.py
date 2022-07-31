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
    

def get_metadata():
    repo = Repo.init(os.getcwd())
    
    return dict(
        reproducibility=dict(
            command_line=f'python {" ".join(sys.argv)}',
            time=time.strftime("%c"),
            cwd=str(Path.cwd()),
            commit=str(repo.commit()),
            remotes=[*repo.remote().urls],
        ),
    )


def get_logger(name):
    params, logger = initialize(
        graphql_endpoint=os.getenv("GRAPHQL_ENDPOINT"),
        charts=get_charts(),
        config="params.yaml",
        load_id=None,
        name=name,
        metadata=get_metadata(),
    )
    return params, logger
