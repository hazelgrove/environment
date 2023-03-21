import os
import sys
import time
from pathlib import Path

from git import Repo


def get_charts():
    update_reward = {
        "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
        "description": "Reward Over Update",
        "data": {"data": "data", "values": []},
        "transform": [{}],
        "mark": "line",
        "encoding": {
            "x": {
                "field": "update",
                "type": "quantitative",
                "title": "Update Number",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
            "y": {
                "field": "mean_episode_rewards",
                "type": "quantitative",
                "title": "Reward",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
        },
        "width": 600,
        "height": 600,
    }
    
    update_eval = {
        "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
        "description": "Evaluation Reward Over Update",
        "data": {"data": "data", "values": []},
        "transform": [{}],
        "mark": "line",
        "encoding": {
            "x": {
                "field": "update",
                "type": "quantitative",
                "title": "Update Number",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
            "y": {
                "field": "eval_reward",
                "type": "quantitative",
                "title": "Evaluation Reward",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
        },
        "width": 600,
        "height": 600,
    }

    update_grad_norm = {
        "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
        "description": "Gradient Norm Over Update",
        "data": {"data": "data", "values": []},
        "transform": [{}],
        "mark": "line",
        "encoding": {
            "x": {
                "field": "update",
                "type": "quantitative",
                "title": "Update Number",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
            "y": {
                "field": "gradient_norm",
                "type": "quantitative",
                "title": "Gradient Norm",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
        },
        "width": 600,
        "height": 600,
    }

    update_fps = {
        "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
        "description": "FPS Over Update",
        "data": {"data": "data", "values": []},
        "transform": [{}],
        "mark": "line",
        "encoding": {
            "x": {
                "field": "update",
                "type": "quantitative",
                "title": "Update Number",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
            "y": {
                "field": "fps",
                "type": "quantitative",
                "title": "Frames per Second",
                "axis": {
                    "titleFontSize": 18,
                    "labelFontSize": 14,
                },
            },
        },
        "width": 600,
        "height": 600,
    }

    return [
        update_reward,
        update_eval,
        update_grad_norm,
        update_fps,
    ]


def get_metadata(repo: Repo):
    return dict(
        reproducibility=dict(
            command_line=f'python {" ".join(sys.argv)}',
            time=time.strftime("%c"),
            cwd=str(Path.cwd()),
            commit=str(repo.commit()),
            remotes=[*repo.remote().urls],
        ),
    )
