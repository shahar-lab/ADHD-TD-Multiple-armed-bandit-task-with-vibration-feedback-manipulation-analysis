### Multiple-armed bandit task with vibration - Response Times results:


rm(list=ls())
#source('./functions/my_packages.R')
load('./data/empirical_data/df.rdata')


### Load libraries:
library(dplyr)
library(ggplot2)
library(effects)
library(brms)
library(rstan)
library(bayestestR)
library(cmdstanr)
library(lme4)
library(emmeans)
library(distributional)
library(ggdist)
library(tidybayes)


### Organize data:

df$condition = relevel(df$condition, ref = 'off')
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)


### RT by delta_exp_value, condition and group:

# Create brm RT model:
rt_model1<-brm(rt ~ delta_exp_value*condition*group +(delta_exp_value*condition| subject),
           data = df|>filter(block_phase=='second_half'),
           family = exgaussian,
           warmup = 2000,
           iter = 3000,    
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:
conditional_effects(rt_model1)
conditions <- make_conditions(rt_model1, "condition")
conditional_effects(rt_model1, "delta_exp_value:group", conditions = conditions)

bayestestR::describe_posterior(rt_model1, ci=(.89))


### RT by condition and group:

# Create brm RT model:
rt_model2<-brm(rt ~ condition*group +(condition| subject),
                 data = df|>filter(block_phase=='second_half'),
                 family = exgaussian,
                 warmup = 2000,
                 iter = 3000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditional_effects(rt_model2)
conditions <- make_conditions(rt_model2, "condition")
conditional_effects(rt_model2, "delta_exp_value:group", conditions = conditions)

bayestestR::describe_posterior(rt_model2, ci=(.89))


### RT by reward_oneback, condition and group:

# Create brm RT model:
rt_model3 <-brm(rt ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
           data = df |> filter(stay == 100),
           family = exgaussian,
           warmup = 500,
           iter = 700,    
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:
conditional_effects(rt_model3)
conditions <- make_conditions(rt_model3, "condition")
conditional_effects(rt_model3, "reward_oneback:group", conditions = conditions)

bayestestR::describe_posterior(rt_model3, ci=(.89))


### RT by delta_exp_value:

# Create brm RT model:
rt_model4<-brm(rt ~ delta_exp_value +(delta_exp_value| subject),
                 data = df|>filter(block_phase=='second_half'),
                 family = exgaussian,
                 warmup = 2000,
                 iter = 3000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditional_effects(rt_model4)
bayestestR::describe_posterior(rt_model4, ci=(.89))


### RT by group:

# Create brm RT model:
rt_model5<-brm(rt ~ group,
                 data = df|>filter(block_phase=='second_half'),
                 family = exgaussian,
                 warmup = 2000,
                 iter = 3000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditional_effects(rt_model5)
bayestestR::describe_posterior(rt_model5, ci=(.89))


### RT by delta_level, condition and group:

# Create brm RT model:
rt_model6<-brm(rt ~ delta_level*condition*group +(delta_level*condition| subject),
                 data = df|>filter(block_phase=='second_half'),
                 family = exgaussian,
                 warmup = 2000,
                 iter = 3000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditions <- make_conditions(rt_model6, "condition")
conditional_effects(rt_model6, "delta_level:group", conditions = conditions)
bayestestR::describe_posterior(rt_model6, ci=(.89))

# emmeans:
em=emmeans::emmeans(rt_model6,~delta_level*condition*group)
cont= emmeans::contrast(em, list('off_adhd'=c(0,0,0,0,0,0,-1,1,0,0,0,0),
                                 'off_td'=c(-1,1,0,0,0,0,0,0,0,0,0,0),
                                 'off'=c(-1,1,0,0,0,0,-1,1,0,0,0,0),
                                 'choice_adhd'=c(0,0,0,0,0,0,0,0,-1,1,0,0),
                                 'choice_td'=c(0,0,-1,1,0,0,0,0,0,0,0,0),
                                 'choice'=c(0,0,-1,1,0,0,0,0,-1,1,0,0),
                                 'outcome_adhd'=c(0,0,0,0,0,0,0,0,0,0,-1,1),
                                 'outcome_td'=c(0,0,0,0,-1,1,0,0,0,0,0,0),
                                 'outcome'=c(0,0,0,0,1,-1,0,0,0,0,-1,1)
))

hpd.summary(cont,0.89)


### RT by delta_level, condition and group, accurate trials:

# Create brm RT model:
rt_model7<-brm(rt ~ delta_level*condition*group +(delta_level*condition| subject),
                 data = df|>filter(block_phase=='second_half')|>filter(accuracy == 1),
                 family = exgaussian,
                 warmup = 5000,
                 iter = 6000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditions <- make_conditions(rt_model7, "condition")
conditional_effects(rt_model7, "delta_level:group", conditions = conditions)
bayestestR::describe_posterior(rt_model7, ci=(.89))


### RT by delta_exp_value, condition and group, accurate trials:

# Create brm RT model:
rt_model8<-brm(rt ~ delta_exp_value*condition*group +(delta_exp_value*condition| subject),
                 data = df|>filter(accuracy == 1),
                 family = exgaussian,
                 warmup = 5000,
                 iter = 6000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditions <- make_conditions(rt_model8, "condition")
conditional_effects(rt_model8, "delta_exp_value:group", conditions = conditions)
bayestestR::describe_posterior(rt_model8, ci=(.89))

