from envs.ast_env import ASTEnv


def main():
    env = ASTEnv(1, [1])

    for i in range(5):
        print("-----------------------")
        print(f"Timestep: {i + 1}")

        obs = env.reset()
        print("Reset environment.")
        print(f"Assignment index: {env.get_state()['assignment']}")
        env.render()
        print()

        done = False
        while not done:
            # TODO: change this to policy + permitted actions
            action = env.action_space.sample()
            while obs["permitted_actions"][action] == 0:
                action = env.action_space.sample()

            print(f"Action taken: {action}")

            obs, reward, done, info = env.step(action)

            env.render()

            print(f"Reward: {reward}\n")
        print("Done!")
    env.close()


if __name__ == "__main__":
    main()
