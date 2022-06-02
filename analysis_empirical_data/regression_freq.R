rm(list=ls())
source('./functions/my_packages.R')
load('G:/My Drive/Xbox/data/empirical_data/df.rdata')


model<-glmer(stay~ reward_oneback*condition_oneback +(reward_oneback*condition_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)


plot(effect('reward_oneback',model))
plot(effect('condition_oneback',model))
plot(effect('reward_oneback:condition_oneback',model,xlevels=2))

summary(model)
anova(model)


