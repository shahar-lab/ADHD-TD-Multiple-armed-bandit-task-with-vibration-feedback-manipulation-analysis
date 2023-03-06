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
ggplot(df|> filter(trial == 1)|>
         #group_by(subject)|> 
         summarise(acc = mean(accuracy)), aes(x = subject, y=acc))+geom_point()

# Create glmer accuracy model

model<-glmer(accuracy~ trial*condition*group +(trial*condition| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


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

# Create brm time model

df$condition = relevel(df$condition, ref = ' vibration off') 
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)

model<-brm(accuracy ~ delta_exp_value*condition*group, 
           data = df ,
           family = bernoulli,
           warmup = 500,
           iter = 700,    
           cores =4,
           chains=4,
           backend='cmdstan')

conditional_effects(model)

conditional_effects(
  model,
  'delta_exp_value:group',
  int_conditions = list(condition=c(' vibration off'))
)

conditional_effects(
  model,
  'delta_exp_value:group',
  int_conditions = list(condition=c(' vibration during outcome'))
)

conditional_effects(
  model,
  'delta_exp_value:group',
  int_conditions = list(condition=c(' vibration during choice'))
)

describe_posterior(model)


