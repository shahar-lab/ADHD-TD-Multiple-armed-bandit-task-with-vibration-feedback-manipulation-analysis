rm(list=ls())
#source('./functions/my_packages.R')
load('./data/empirical_data/df.rdata')

# Load libraries:
library(dplyr)
library(ggplot2)
library(effects)
library(brms)
library(rstan)
library(bayestestR)
library(cmdstanr)


# Organize data:

df$condition = relevel(df$condition, ref = 'off')
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)


# Create brm response time by difficulty model:

time_model1<-brm(rt ~ delta_exp_value*condition*group +(delta_exp_value*condition| subject),
           data = df|>filter(block_phase=='second_half'),
           family = exgaussian,
           warmup = 2000,
           iter = 3000,    
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:

conditional_effects(time_model1)
conditions <- make_conditions(time_model1, "condition")
conditional_effects(time_model1, "delta_exp_value:group", conditions = conditions)

bayestestR::describe_posterior(time_model1, ci=(.89))
#describe_posterior(time_model1)

# Create brm time by reward model:

time_model2 <-brm(rt ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
           data = df |> filter(stay == 100),
           family = exgaussian,
           warmup = 500,
           iter = 700,    
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:

conditional_effects(time_model2)
conditions <- make_conditions(time_model2, "condition")
conditional_effects(time_model2, "reward_oneback:group", conditions = conditions)

describe_posterior(time_model2)




