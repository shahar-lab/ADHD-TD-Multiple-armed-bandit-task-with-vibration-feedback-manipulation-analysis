rm(list=ls())
#source('./functions/my_packages.R')
load('./data/empirical_data/df.rdata')

# Load libraries:
library(ggplot2)
library(effects)
library(brms)
library(rstan)
library(bayestestR)
library(cmdstanr)

# View accuracy results:
#library(dplyr)
#df = 
#df |>
#  group_by(trial,condition,group) |>
#  summarise(acc = mean(accuracy))

ggplot(df, aes(x = trial, y=acc))+geom_line()
ggplot(df, aes(x = trial, y=acc,color=group]))+geom_point()+geom_line()

# Create glmer accuracy model

model<-glmer(accuracy~ trial*condition*group +(trial*condition| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)


plot(effect('trial',model))
plot(effect('condition',model))
plot(effect('condition:group',model,xlevels=2))
plot(effect('group',model))


summary(model)
anova(model)

# Create brm accuracy model

model<-brm(accuracy ~ trial*condition*group +(trial*condition| subject), 
           data = df ,
           family = bernoulli,
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           backend='cmdstan')

describe_posterior(model)


