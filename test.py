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
            for i in len(obs["permitted_actions"]):
                if obs["permitted_actions"][i] == 1:
                    action = i

            print(f"Action taken: {action}")

            obs, reward, done, info = env.step(action)

            env.render()

            print(f"Reward: {reward}\n")
        print("Done!")
    env.close()


if __name__ == "__main__":
    main()
