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


# Organize data:

df$condition = relevel(df$condition, ref = 'off') 
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)

# Create glmer accuracy model:

accuracy_model1<-glmer(accuracy~ trial*condition*group +(trial*condition| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

# View results:

plot(effect('trial',accuracy_model1))
plot(effect('condition',accuracy_model1))
plot(effect('condition:group',accuracy_model1,xlevels=2))
plot(effect('group',accuracy_model1))

summary(accuracy_model1)
anova(accuracy_model1)

# Create brm accuracy model:

accuracy_model2<-brm(accuracy ~ trial*condition*group +(trial*condition| subject), 
           data = df ,
           family = bernoulli,
           warmup = 1000,
           iter = 2000,    
           cores =4,
           chains=4,
           backend='cmdstan')

# View results:
conditional_effects(accuracy_model2)
conditions <- make_conditions(accuracy_model2, "condition")
conditional_effects(accuracy_model2, "trial:group", conditions = conditions)

describe_posterior(accuracy_model2)


# Create brm accuracy by difficulty model:

accuracy_model3<-brm(accuracy ~ delta_exp_value*condition*group, 
           data = df ,
           family = bernoulli,
           warmup = 500,
           iter = 700,    
           cores =4,
           chains=4,
           backend='cmdstan')

# View results:
conditional_effects(accuracy_model3)
conditions <- make_conditions(accuracy_model3, "condition")
conditional_effects(accuracy_model3, "delta_exp_value:group", conditions = conditions)

describe_posterior(accuracy_model3)


