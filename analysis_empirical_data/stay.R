### Multiple-armed bandit task with vibration - Stay results:


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
library(emmeans)
library(distributional)
library(ggdist)
library(tidybayes)


### View stay results:
#df = 
#  df |>
#  group_by(reward_oneback,condition_oneback,group) |>
#  summarise(st = mean(stay))

#ggplot(df, aes(x = reward_oneback, y=st))+geom_line()
#ggplot(df, aes(x = reward_oneback, y=st,color=group))+geom_point()+geom_line()


### Organize data:
df$condition = relevel(df$condition, ref = 'off') 
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)
df$reward_oneback = factor(df$reward_oneback,levels = c(0,1),labels = c('unrewarded','rewarded'))
contrasts(df$reward_oneback)


### Stay by reward_oneback, condition_oneback and group:

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


### Stay by reward_oneback, condition and group:

# Create brm stay model:
stay_model2<-brm(stay ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
           data = df,
           #family = bernoulli,
           warmup = 5000,
           iter = 6000,    
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:
conditional_effects(stay_model2)
conditions <- make_conditions(stay_model2, "condition")
conditional_effects(stay_model2, "reward_oneback:group", conditions = conditions)

bayestestR::describe_posterior(stay_model2, ci=(.89))

#emmeans:
em=emmeans::emmeans(stay_model2,~reward_oneback*condition*group)
cont= emmeans::contrast(em, list('outcome_vs_off'=c(0.5,0,-0.5,-0.5,0,0.5),
                                 'choice_vs_off'=c(0.5,-0.5,0,-0.5,0.5,0),
                                 'off'=c(-1,0,0,1,0,0),
                                 'choice'=c(0,-1,0,0,1,0),
                                 'outcome'=c(0,0,-1,0,0,1)
))

hpd.summary(cont,0.89)


### Stay by reward_oneback:

# Create brm stay model:
stay_model3<-brm(stay ~ reward_oneback +(reward_oneback| subject), 
                 data = df,
                 #family = bernoulli,
                 warmup = 5000,
                 iter = 6000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditional_effects(stay_model3)
bayestestR::describe_posterior(stay_model3, ci=(.89))


### Stay - Trials 1 - 25:

# Create brm stay model:
stay_model4<-brm(stay ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
                 data = df|>filter(trial >= 1 & trial <= 25),
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

bayestestR::describe_posterior(stay_model4, ci=(.89))



### Irrelevant learning models:

### Stay key by reward_oneback, condition_oneback and group:

# Create brm stay key model:
stay_key_model1<-glmer(stay_key~ reward_oneback*condition_oneback*group +(1|subject),
                   data = df, 
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

# View results:
plot(effect('reward_oneback',stay_key_model1))
plot(effect('condition_oneback',stay_key_model1))
plot(effect('group',stay_key_model1))
plot(effect('reward_oneback:condition_oneback',stay_key_model1))
plot(effect('reward_oneback:condition_oneback:group',stay_key_model1,xlevels=2))

summary(stay_key_model1)
anova(stay_key_model1)


### Stay key by reward_oneback, condition and group:

# Create brm stay key model:
stay_key_model2<-brm(stay_key ~ reward_oneback*condition*group +(reward_oneback*condition| subject),
                 data = df|>filter(reward_oneback = factor(reward_oneback)),
                 family = bernoulli,
                 warmup = 1000,
                 iter = 2000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditional_effects(stay_key_model2)
conditions <- make_conditions(stay_key_model2, "condition")
conditional_effects(stay_key_model2, "reward_oneback:group", conditions = conditions)

bayestestR::describe_posterior(stay_key_model2, ci=(.89))


### Stay key by reward_oneback, condition and group, no filter:

# Create brm stay key model:
stay_key_model3<-brm(stay ~ reward_oneback*condition*group +(reward_oneback*condition| subject), 
                 data = df,
                 #family = bernoulli,
                 warmup = 2000,
                 iter = 3000,    
                 cores = 4,
                 chains = 4,
                 backend='cmdstan')

# View results:
conditional_effects(stay_key_model3)
conditions <- make_conditions(stay_key_model3, "condition")
conditional_effects(stay_key_model3, "reward_oneback:group", conditions = conditions)

bayestestR::describe_posterior(stay_key_model3, ci=(.89))

