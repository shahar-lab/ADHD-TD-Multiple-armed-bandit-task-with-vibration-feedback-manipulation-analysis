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


# View stay results:
df = 
  df |>
  group_by(reward_oneback,condition_oneback,group) |>
  summarise(st = mean(stay))

ggplot(df, aes(x = reward_oneback, y=st))+geom_line()
ggplot(df, aes(x = reward_oneback, y=st,color=group))+geom_point()+geom_line()

# Create glmer accuracy model

model<-glmer(stay~ reward_oneback*condition_oneback*group +(reward_oneback*condition_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)


plot(effect('reward_oneback',model))
plot(effect('condition_oneback',model))
plot(effect('group',model))
plot(effect('reward_oneback:condition_oneback:group',model,xlevels=2))


summary(model)
anova(model)

# Create brm accuracy model

model<-brm(stay ~ reward_oneback*condition_oneback*group +(reward_oneback*condition_oneback| subject), 
           data = df ,
           family = bernoulli,
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           backend='cmdstan')

describe_posterior(model)


