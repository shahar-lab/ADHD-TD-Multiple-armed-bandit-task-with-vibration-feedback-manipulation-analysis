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
library(lme4)
library(emmeans)

# View accuracy results:
#library(dplyr)
#df = 
#df |>
#  group_by(trial,condition,group) |>
#  summarise(acc = mean(accuracy))

#ggplot(df, aes(x = trial, y=acc))+geom_line()
#ggplot(df|> filter(trial == 1)|>
         #group_by(subject)|> 
#         summarise(acc = mean(accuracy)), aes(x = subject, y=acc))+geom_point()


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
           #family = bernoulli,
           warmup = 1000,
           iter = 1200,    
           cores = 4,
           chains = 4,
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
           cores = 4,
           chains = 4,
           backend='cmdstan')

# View results:
conditional_effects(accuracy_model3)
conditions <- make_conditions(accuracy_model3, "condition")
conditional_effects(accuracy_model3, "delta_exp_value:group", conditions = conditions)

describe_posterior(accuracy_model3)

### Accuracy by phases:

# Create brm accuracy by phases model:

accuracy_model4<-brm(accuracy ~ block_phase*condition*group +(block_phase*condition| subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# emmeans

em=emmeans::emmeans(accuracy_model4,~block_phase*condition*group)
cont= emmeans::contrast(em, list('off_adhd'=c(0,0,0,0,0,0,-1,1,0,0,0,0),
                                 'off_td'=c(-1,1,0,0,0,0,0,0,0,0,0,0),
                                 'off'=c(-1,1,0,0,0,0,1,-1,0,0,0,0),
                                 'choice_adhd'=c(0,0,0,0,0,0,0,0,-1,1,0,0),
                                 'choice_td'=c(0,0,-1,1,0,0,0,0,0,0,0,0),
                                 'choice'=c(0,0,-1,1,0,0,0,0,-1,1,0,0),
                                 'outcome_adhd'=c(0,0,0,0,0,0,0,0,0,0,-1,1),
                                 'outcome_td'=c(0,0,0,0,-1,1,0,0,0,0,0,0),
                                 'outcome'=c(0,0,0,0,-1,1,0,0,0,0,1,-1)
                                 ))

hpd.summary(cont,0.89)
eff_size(cont, method = "pd", edf = 8)

# View results:
conditional_effects(accuracy_model4)
conditions <- make_conditions(accuracy_model4, "condition")
conditional_effects(accuracy_model4, "block_phase:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model4, ci=(.89))
#describe_posterior(accuracy_model4)


# Accuracy - Vibration During Outcome:

accuracy_model5<-brm(accuracy ~ block_phase*group +(block_phase| subject), 
                     data = df|>filter(condition=='outcome'),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model5)
bayestestR::describe_posterior(accuracy_model5, ci=(.89))


# Accuracy - Phases:

accuracy_model6<-brm(accuracy ~ block_phase +(block_phase| subject), 
                     data = df,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

conditional_effects(accuracy_model6)
bayestestR::describe_posterior(accuracy_model6, ci=(.89))

# Accuracy - Groups:

accuracy_model7<-brm(accuracy ~ group, 
                     data = df,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

conditional_effects(accuracy_model7)
bayestestR::describe_posterior(accuracy_model7, ci=(.89))


# Create brm accuracy by difficulty model:

accuracy_model8<-brm(accuracy ~ delta_exp_value*condition*group +(delta_exp_value*condition| subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model8)
conditions <- make_conditions(accuracy_model8, "condition")
conditional_effects(accuracy_model8, "delta_exp_value:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model8, ci=(.89))

# Accuracy - first 5 trials

accuracy_model9<-brm(accuracy ~ trial*condition*group +(delta_exp_value*condition| subject), 
                     data = df %>% filter(trial==1 | trial==2),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')




# Accuracy - first 5 trials

accuracy_model9<-brm(accuracy ~ trial*condition*group +(delta_exp_value*condition| subject), 
                     data = df|>filter(trial==1|trial==2|trial==3|trial==4|trial==5),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model9)
conditions <- make_conditions(accuracy_model9, "condition")
conditional_effects(accuracy_model9, "trial:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model9, ci=(.89))

# Accuracy - first 10 trials

accuracy_model10<-brm(accuracy ~ trial*condition*group +(delta_exp_value*condition| subject), 
                     data = df|>filter(df$trial==1|df$trial==2|df$trial==3|df$trial==4|df$trial==5|df$trial==6|df$trial==7|df$trial==8|df$trial==9|df$trial==10),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model10)
conditions <- make_conditions(accuracy_model10, "condition")
conditional_effects(accuracy_model10, "trial:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model10, ci=(.89))
