import gym

def main():
    env = gym.make("gym_basic:ast-v0")
    env.init(1, [1])
    
    for i in range(5):
        print("-----------------------")
        print(f"Timestep: {i + 1}")
        
        env.reset()
        print("Reset environment.\n")
        
        done = False
        while not done:
            action = env.action_space.sample()
            print(f"Action taken: {action}")

            obs, reward, done, info = env.step(action)
            
            env.render()
            
            print(f"Reward: {reward}\n")
        print("Done!")
    env.close()

if __name__ == '__main__':
    main()