from run_logger import create_sweep


def sweep(
    log_name: str,
    config_path: str,
    random_search: bool = False,
):
    with config_path.open() as f:
        partial_config = yaml.load(f, yaml.FullLoader)
​
    config: "dict[str, Any]" = dict(
        name=name,
        repo=Repo("."),
        **kwargs,
        **{
            k: (tune.choice(v) if random_search else tune.grid_search(v))
            if isinstance(v, list)
            else v
            for k, v in partial_config.items()
        },
    )
