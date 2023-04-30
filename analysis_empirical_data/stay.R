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
library(lme4)

# View stay results:
df = 
  df |>
  group_by(reward_oneback,condition_oneback,group) |>
  summarise(st = mean(stay))

ggplot(df, aes(x = reward_oneback, y=st))+geom_line()
ggplot(df, aes(x = reward_oneback, y=st,color=group))+geom_point()+geom_line()

# Organize data:
df$condition = relevel(df$condition, ref = 'off') 
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)

# Create glmer stay model:

stay_model1<-glmer(stay~ reward_oneback*condition_oneback*group +(1|subject),
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

# View results:

plot(effect('reward_oneback',stay_model1))
plot(effect('condition_oneback',stay_model1))
plot(effect('group',stay_model1))
plot(effect('reward_oneback:condition_oneback:group',stay_model1,xlevels=2))


summary(stay_model1)
anova(stay_model1)


# Create brm stay model:

stay_model2<-brm(stay ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
           data = df,
           #family = bernoulli,
           warmup = 2000,
           iter = 3000,    
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:

conditional_effects(stay_model2)
conditions <- make_conditions(stay_model2, "condition")
conditional_effects(stay_model2, "reward_oneback:group", conditions = conditions)

#describe_posterior(stay_model2)
bayestestR::describe_posterior(stay_model2, ci=(.89))


# Stay key model1 - irrelevant learning

stay_model3<-glmer(stay_key~ reward_oneback*condition_oneback*group +(1|subject),
                   data = df, 
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

# View results:

plot(effect('reward_oneback',stay_model3))
plot(effect('condition_oneback',stay_model3))
plot(effect('group',stay_model3))
plot(effect('reward_oneback:condition_oneback',stay_model3))
plot(effect('reward_oneback:condition_oneback:group',stay_model3,xlevels=2))


summary(stay_model3)
anova(stay_model3)

# Stay key model1 - irrelevant learning
stay_model4<-brm(stay ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
                 data = df,
                 #family = bernoulli,
                 warmup = 2000,
                 iter = 3000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:

conditional_effects(stay_model4)
conditions <- make_conditions(stay_model4, "condition")
conditional_effects(stay_model4, "reward_oneback:group", conditions = conditions)

#describe_posterior(stay_model4)
bayestestR::describe_posterior(stay_model4, ci=(.89))
