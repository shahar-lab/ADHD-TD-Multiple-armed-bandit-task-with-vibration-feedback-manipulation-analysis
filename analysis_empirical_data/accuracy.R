### Multiple-armed bandit task with vibration - Accuracy results:


rm(list=ls())
#source('./functions/my_packages.R')
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


### View accuracy results:
#library(dplyr)
#df = 
#df |>
#  group_by(trial,condition,group) |>
#  summarise(acc = mean(accuracy))

#ggplot(df, aes(x = trial, y=acc))+geom_line()
#ggplot(df|> filter(trial == 1)|>
         #group_by(subject)|> 
#         summarise(acc = mean(accuracy)), aes(x = subject, y=acc))+geom_point()


### Organize data:

df$condition = relevel(df$condition, ref = 'off') 
contrasts(df$condition)
df$group = relevel(df$group, ref = 'td')
contrasts(df$group)


### Accuracy by trial, condition and group:

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


### Accuracy by trial, condition and group:

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

bayestestR::describe_posterior(accuracy_model2, ci=(.89))


### Accuracy by block_phase, condition and group:

# Create brm accuracy model:
accuracy_model3<-brm(accuracy ~ block_phase*condition*group +(block_phase*condition| subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model3)
conditions <- make_conditions(accuracy_model3, "condition")
conditional_effects(accuracy_model3, "block_phase:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model3, ci=(.89))


### Accuracy by condition and group:

# Create brm accuracy model:
accuracy_model4<-brm(accuracy ~ condition*group +(condition| subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 5000,
                     iter = 6000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# emmeans:
normalize_weights <- function(...) {
  w <- c(...) 
  w_sgn <- factor(sign(w)) 
  w_abs <- abs(w) 
  w / ave(x = w_abs, w_sgn, FUN = sum)}

em=emmeans::emmeans(accuracy_model4,~condition*group)
cont= emmeans::contrast(em, list('outcome_vs_off'=c(0.5,0,-0.5,-0.5,0,0.5),
                                 'choice_vs_off'=c(0.5,-0.5,0,-0.5,0.5,0),
                                 'off'=c(-1,0,0,1,0,0),
                                 'choice'=c(0,-1,0,0,1,0),
                                 'outcome'=c(0,0,-1,0,0,1)
                                 ))

hpd.summary(cont,0.89)
eff_size(cont, method = "pd", edf = 8)

# View results:
conditional_effects(accuracy_model4)
conditions <- make_conditions(accuracy_model4, "condition")
conditional_effects(accuracy_model4, "block_phase:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model4, ci=(.89))


### Accuracy - Intercept:

# Create brm accuracy model:
accuracy_model5<-brm(accuracy ~ 1+(1|subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 5000,
                     iter = 6000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
bayestestR::describe_posterior(accuracy_model5, ci=(.89))

#single distrbution 
params = insight::get_parameters(accuracy_model5)
params |> select(b_Intercept) |> ggplot(aes(x = b_Intercept)) + stat_halfeye() + ggtitle("Accuracy Intercept") + scale_x_continuous(limits = c(0.45, 0.6))

hist(params$b_Intercept)


### Accuracy - Vibration During Outcome:

# Create brm accuracy model:
accuracy_model6<-brm(accuracy ~ block_phase*group +(block_phase| subject), 
                     data = df|>filter(condition=='outcome'),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model6)
bayestestR::describe_posterior(accuracy_model6, ci=(.89))


### Accuracy by block_phase:

# Create brm accuracy model:
accuracy_model7<-brm(accuracy ~ block_phase +(block_phase| subject), 
                     data = df,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model7)
bayestestR::describe_posterior(accuracy_model7, ci=(.89))


### Accuracy by group:

# Create brm accuracy model:
accuracy_model8<-brm(accuracy ~ group, 
                     data = df,
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model8)
bayestestR::describe_posterior(accuracy_model8, ci=(.89))



### Accuracy by delta_exp_value, condition and group:

# Create brm accuracy model:
accuracy_model9<-brm(accuracy ~ delta_exp_value*condition*group +(delta_exp_value*condition| subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 5000,
                     iter = 6000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model9)
conditions <- make_conditions(accuracy_model9, "condition")
conditional_effects(accuracy_model9, "delta_exp_value:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model9, ci=(.89))

# emmeans:
em = emtrends(accuracy_model9, ~ delta_exp_value * condition * group, 
         var = c("delta_exp_value"), 
         at = list(condition = c('off','choice','outcome'), group = c('td','adhd')))

cont= emmeans::contrast(em, list('adhd_off_vs_other'=c(0,0,0,-1,0.5,0.5)))
cont |> gather_emmeans_draws() |> describe_posterior()

hpd.summary(cont,0.89)


### Accuracy by delta_level (easy/hard), condition and group:

# Create brm accuracy model:
accuracy_model10<-brm(accuracy ~ delta_level*condition*group +(delta_level*condition| subject), 
                     data = df ,
                     #family = bernoulli,
                     warmup = 5000,
                     iter = 6000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model10)
conditions <- make_conditions(accuracy_model10, "condition")
conditional_effects(accuracy_model10, "delta_level:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model10, ci=(.89))

# emmeans:
em=emmeans::emmeans(accuracy_model10,~delta_level*condition*group)
cont= emmeans::contrast(em, list('off_adhd'=c(0,0,0,0,0,0,-1,1,0,0,0,0),
                                 'choice_adhd'=c(0,0,0,0,0,0,0,0,-1,1,0,0),
                                 'outcome_adhd'=c(0,0,0,0,0,0,0,0,0,0,-1,1),
                                 'choice_vs_off'=c(0,0,0,0,0,0,1,-1,-1,1,0,0),
                                 'outcome_vs_off'=c(0,0,0,0,0,0,1,-1,0,0,-1,1)
))

hpd.summary(cont,0.89)



### Accuracy - first 5 trials:

# Create brm accuracy model:
accuracy_model11<-brm(accuracy ~ trial*condition*group +(delta_exp_value*condition| subject), 
                     data = df |> filter(trial >= 1 & trial <= 5),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model11)
conditions <- make_conditions(accuracy_model11, "condition")
conditional_effects(accuracy_model11, "trial:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model11, ci=(.89))


### Accuracy - first 10 trials:

# Create brm accuracy model:
accuracy_model12<-brm(accuracy ~ trial*condition*group +(delta_exp_value*condition| subject), 
                     data = df|>filter(trial >= 1 & trial <= 10),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model12)
conditions <- make_conditions(accuracy_model12, "condition")
conditional_effects(accuracy_model12, "trial:group", conditions = conditions)

bayestestR::describe_posterior(accuracy_model12, ci=(.89))


### Accuracy - trials 25-50:

# Create brm accuracy model:
accuracy_model13<-brm(accuracy ~ condition*group +(condition| subject), 
                     data = df|>filter(trial >= 25 & trial <= 50),
                     #family = bernoulli,
                     warmup = 2000,
                     iter = 3000,    
                     cores = 4,
                     chains = 4,
                     backend='cmdstan')

# View results:
conditional_effects(accuracy_model13)
bayestestR::describe_posterior(accuracy_model13, ci=(.89))

