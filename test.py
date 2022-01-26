import multiprocessing as mp
import gym
from stable_baselines3.common.vec_env.base_vec_env import (
    CloudpickleWrapper,
    VecEnv,
    VecEnvIndices,
    VecEnvObs,
    VecEnvStepReturn,
)


def f():
    return gym.make("gym_basic:test-v0")


def _worker(
    remote: mp.connection.Connection,
    parent_remote: mp.connection.Connection,
    env_fn_wrapper: CloudpickleWrapper,
) -> None:
    # Import here to avoid a circular import
    from stable_baselines3.common.env_util import is_wrapped

    parent_remote.close()
    env = env_fn_wrapper.var()
    print(env)

    while True:
        try:
            cmd, data = remote.recv()
            remote.send((env.observation_space, env.action_space))
            # env(data)
            # remote.send(("obs", "action"))
        #           if cmd == "step":
        #               observation, reward, done, info = env.step(data)
        #               if done:
        #                   # save final observation where user can get it, then reset
        #                   info["terminal_observation"] = observation
        #                   observation = env.reset()
        #               remote.send((observation, reward, done, info))
        #           elif cmd == "seed":
        #               remote.send(env.seed(data))
        #           elif cmd == "reset":
        #               observation = env.reset()
        #               remote.send(observation)
        #           elif cmd == "render":
        #               remote.send(env.render(data))
        #           elif cmd == "close":
        #               env.close()
        #               remote.close()
        #               break
        #           elif cmd == "get_spaces":
        #           elif cmd == "env_method":
        #               method = getattr(env, data[0])
        #               remote.send(method(*data[1], **data[2]))
        #           elif cmd == "get_attr":
        #               remote.send(getattr(env, data))
        #           elif cmd == "set_attr":
        #               remote.send(setattr(env, data[0], data[1]))
        #           elif cmd == "is_wrapped":
        #               remote.send(is_wrapped(env, data))
        #           else:
        #               raise NotImplementedError(f"`{cmd}` is not implemented in the worker")
        except EOFError:
            break


def main():
    n_envs = 2
    env_fns = [f for _ in range(n_envs)]
    forkserver_available = "forkserver" in mp.get_all_start_methods()
    start_method = "forkserver" if forkserver_available else "spawn"
    ctx = mp.get_context(start_method)
    remotes, work_remotes = zip(*[ctx.Pipe() for _ in range(n_envs)])
    processes = []
    for work_remote, remote, env_fn in zip(work_remotes, remotes, env_fns):
        args = (work_remote, remote, CloudpickleWrapper(env_fn))
        # daemon=True: if the main process crashes, we should not cause things to hang
        process = ctx.Process(
            target=_worker, args=args, daemon=True
        )  # pytype:disable=attribute-error
        process.start()
        processes.append(process)
        work_remote.close()

    remotes[0].send(("get_spaces", 4))
    observation_space, action_space = remotes[0].recv()

    print(observation_space, action_space)
