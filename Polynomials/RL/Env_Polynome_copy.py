# -*- coding: utf-8 -*-
"""
Created on Fri Dec 30 10:36:01 2022

@author: gmgno
"""

# We create a custom environment defined by:
#
#    actions: recommend items from specific week
#    observations: the difficulty ratings of the items and the score get by the user for that item
#    rewards: The global score equals to threshold and if the reccomendation follows the ability 
#            of the subject to reach the threshold.
#
# Each episode for an agent will simulate one user's ratings.

import sys
import time
import gym
from gym import spaces
import random
import pandas as pd
import numpy as np
import os

os.chdir('Your path here')

difficulty = pd.read_csv('Difficulty_Polynome.csv')
difficulty = difficulty.iloc[: , 1:]

score_data = pd.read_excel('Score_Person_item.xlsx') 
#score_data = score_data.iloc[: , 1:]
score_data.isnull().sum()
score_data = score_data.dropna(how='any',axis=0) 


threshold = [10.5, 15.4, 19.3]
ability_threshold = [-0.15, 1.05, 1.60]
max_score = [7,5,6,4]


reward_depleted = -1
reward_done = 100

env_config = {
    "dataset": score_data,
    "difficulty": difficulty.values[0],
    "th_level" : np.array(list(range(0,3)))
}

########################
### ENVIRONMENT CREATION
########################

class Polynome(gym.Env):
    def __init__ (self, env_config):
        
        self.difficulty = env_config["difficulty"]
        self.observation_space = spaces.Discrete(score_data.shape[1])
        self.action_space = spaces.Discrete(score_data.shape[1])
        # load the dataset
        self.dataset = env_config["dataset"]
        # load the thresholds
        self.th_level = env_config["th_level"]
        self.length = 10
        
    
    def reset(self):
        self.length = 10
        self.actions_used = []
        self.reward_action_used = 0 
        self.reward_difficulty = 0
        self.collected_reward = 0
        self.collected_score = 0
        # select a random user to simulate
        user = random.randint(0, len(score_data) -1)
        self.user_ = self.dataset.iloc[[user]]
        self.user_extr = np.array(self.user_)[0]
        self.item_selected = 0
        level = random.choice(self.th_level)
        self.threshold = threshold[level]
        self.ability_threshold = ability_threshold[level]
 
        
        return int(self.item_selected)
    
    
    def step(self, action):
        
        assert action in self.action_space, action
        
        self.reward_action_used = 0
        self.reward_difficulty = 0
        
        if action in self.actions_used:
            item = None
            self.reward_action_used += reward_depleted    
        else:
            item = self.user_extr[action]
            self.collected_score += item
            item_difficulty = self.difficulty[action]
            if item_difficulty <= self.ability_threshold:
                self.reward_difficulty += 10
            else: 
                self.reward_difficulty += 0    
        if item == max_score[action]:
            self.actions_used.append(action)
        if self.collected_score >= self.threshold or self.length == 0:
            done = True
        else: 
            done = False
        if self.collected_score >= self.threshold:
            self.collected_reward += self.reward_action_used + self.reward_difficulty + reward_done
        else: 
            self.collected_reward += self.reward_action_used + self.reward_difficulty
        
        self.length -= 1
        
        info = { "item": action, "collected_reward": self.collected_reward, "score": self.collected_score, 
                'threshold' : self.threshold, 'ability' : self.ability_threshold, 'actions_used' : self.actions_used,
                'length' : self.length }
    
        return int(self.item_selected), self.collected_reward, done, info
    
    def render(self, mode='human'):
        pass
   
env = Polynome(env_config)
env.reset()
env.step(0)
   
possible_actions = [i for i in range(env.action_space.n)]  
env.observation_space

print(env.action_space)
print(env.observation_space)

##### RANDOM CHOICE SELECTION

episode_reward = []
episodes = 1000
for episode in range(episodes):
    done = False
    rewards = []
    state = env.reset()
    while not done:
        action = env.action_space.sample()
        #print('action:', action)
        state, reward, done, info = env.step(action)
        rewards.append(reward)
    episode_reward.append(sum(rewards))
    print(f"\rEpisode {episode}/{episodes}, total reward: {episode_reward[-1]}", end="")  # to print in same line:, end="")
    sys.stdout.flush()
    pass
print(f"\nAvg reward over {episodes} episodes: {np.mean(episode_reward)} std_episodes: {np.std(episode_reward)}")


###############################################
### IMPLEMENT A REINFORCEMENT LEARNING SOLUTION
###############################################

from stable_baselines3 import PPO, A2C
from stable_baselines3.common.env_checker import check_env
from stable_baselines3.common.evaluation import evaluate_policy
import wandb

check_env(env) # Checking the consistence of the environment


###############################################
#### IMPLEMENTING THE RL SOLUTION
###############################################

Polynome_env = Polynome(env_config)
#First_week_env = DummyVecEnv([lambda: First_week_env])

RL_config = {
    "policy": 'MlpPolicy',
    "total_timesteps": 100000
}

run = wandb.init(
    project="Polynome_PPO",
    config= RL_config,
    sync_tensorboard=True,  # automatically upload SB3's tensorboard metrics to W&B
    #save_code=True,
)


agent = PPO(policy=RL_config['policy'], env= Polynome_env, verbose=1,
            learning_rate = 0.000001,
            #policy_kwargs= dict(net_arch=[64, 32]),
            tensorboard_log= f"tensorboard_logs/{run.id}"
            )

# start the training
t0 = time.time()
best_mean_reward = np.NINF

while True:
    agent.learn(total_timesteps= RL_config['total_timesteps']) 
    
    # Evaluate the agent
    mean_reward, std_reward = evaluate_policy(agent, agent.get_env(), n_eval_episodes=5)
    print(f"time {time.time()-t0}, avg reward: {mean_reward} ± {std_reward}")

    if mean_reward > best_mean_reward:
        agent.save("bestPP0.agent")
        print("new best agent saved")
        best_mean_reward = mean_reward
    if time.time()-t0 >= 3600:
        break
        
run.finish()


model = PPO.load("bestPP0.agent")

episode_reward = []
episodes = 1000
for episode in range(episodes):
    done = False
    rewards = []
    state = env.reset()
    while not done:
        action, _state = model.predict(state)
        #print('action:', action)
        state, reward, done, info = env.step(action)
        rewards.append(reward)
    episode_reward.append(sum(rewards))
    print(f"\rEpisode {episode}/{episodes}, total reward: {episode_reward[-1]}", end="")  # to print in same line:, end="")
    sys.stdout.flush()
    pass
print(f"\nAvg reward over {episodes} episodes: {np.mean(episode_reward)} std_episodes: {np.std(episode_reward)}")


######################################
############# A2C IMPLEMENTATION #####
######################################

run = wandb.init(
    project="Polynome_A2C",
    config= RL_config,
    sync_tensorboard=True,  # automatically upload SB3's tensorboard metrics to W&B
    #save_code=True,
)


agent_A2C = A2C(policy=RL_config['policy'], env= Polynome_env, verbose=1,
            learning_rate = 0.000001,
            #policy_kwargs= dict(net_arch=[64, 32]),
            tensorboard_log= f"tensorboard_logs/{run.id}"
            )

# start the training
t0 = time.time()
best_mean_reward = np.NINF

while True:
    agent_A2C.learn(total_timesteps= RL_config['total_timesteps']) 
    
    # Evaluate the agent
    mean_reward, std_reward = evaluate_policy(agent_A2C, agent_A2C.get_env(), n_eval_episodes=5)
    print(f"time {time.time()-t0}, avg reward: {mean_reward} ± {std_reward}")

    if mean_reward > best_mean_reward:
        agent_A2C.save("bestA2C.agent")
        print("new best agent saved")
        best_mean_reward = mean_reward
    if time.time()-t0 >= 3600:
        break
        
run.finish()


model = A2C.load("bestA2C.agent")

episode_reward = []
episodes = 1000
for episode in range(episodes):
    done = False
    rewards = []
    state = env.reset()
    while not done:
        action, _state = model.predict(state)
        #print('action:', action)
        state, reward, done, info = env.step(action)
        rewards.append(reward)
    episode_reward.append(sum(rewards))
    print(f"\rEpisode {episode}/{episodes}, total reward: {episode_reward[-1]}", end="")  # to print in same line:, end="")
    sys.stdout.flush()
    pass
print(f"\nAvg reward over {episodes} episodes: {np.mean(episode_reward)} std_episodes: {np.std(episode_reward)}")


