import os

import numpy as np
from run_logger import initialize


def get_charts():
    update_reward = {
        "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
        "description": "Mean Episode Reward Over Update",
        "data": {"data": "data", "values": []},
        "transform": [{}],
        "mark": "line",
        "encoding": {
            "x": {"field": "update", "type": "quantitative"},
            "y": {"field": "reward", "type": "quantitative"},
        },
    }

    return [
        update_reward,
    ]


def get_logger():
    config = "logger_config.yaml"
    charts = get_charts()

    params, logger = initialize(
        graphql_endpoint=os.getenv("GRAPHQL_ENDPOINT"),
        config=config,
        charts=charts,
        load_id=None,
    )

    return params, logger
