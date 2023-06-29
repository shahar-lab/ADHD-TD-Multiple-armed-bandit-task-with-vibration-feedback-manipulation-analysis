### Multiple-armed bandit task with vibration - Accuracy results:


rm(list=ls())
load('./data/empirical_data/df.rdata')

### Load libraries:
library(ggplot2)
library(effects)
library(brms)
library(rstan)
library(bayestestR)
library(cmdstanr)
library(lme4)
library(emmeans)
library(dplyr)
library(distributional)
library(ggdist)
library(tidybayes)



### Organize data:

df$group = relevel(df$group, ref = 'td')
contrasts(df$group)<-c(-1,1)
contrasts(df$group)

df$accuracy = df$accuracy*100


#### Create brm accuracy model -------------

#specific the model
f_model = accuracy ~   1 + group + (1 | subject)

get_prior(f_model,data = df)

myprior = c(
  prior(normal(50, 25), class = Intercept),
  
  prior(normal(0, 10),  class = b        , coef = 'groupadhd'),

  prior(student_t(10,0,50), class = sigma )
)


model<-brm(f_model, 
           data   = df ,
           warmup = 10000,
           iter   = 11000,    
           cores  = 4,
           chains = 4,
           backend='cmdstan')


save(model,file='./data/regression/acc~1+group.rdata')



