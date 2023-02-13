import ray
import os
import ctypes
from ray import tune


def trainable(config):
    astclib = ctypes.CDLL(
        "/RL_env/clib/astclib.so"
    )


def sweep():
    ray.init()
    num_cpus = os.cpu_count()
    analysis = ray.tune.run(
        trainable,
        config = {},
        resources_per_trial={"cpu": num_cpus, "gpu": 1},
        fail_fast="raise",
    )
    print(analysis.stats())
    

if __name__ == "__main__":
    sweep()