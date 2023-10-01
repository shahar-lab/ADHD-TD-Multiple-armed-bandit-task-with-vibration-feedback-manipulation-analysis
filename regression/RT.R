library(brms)
library(brms.exgaussian)

myformula=brmsformula(
  rt    ~ 0+Intercept+group*condition*delta_exp_value+(0+Intercept+condition*delta_exp_value|subject),
  tau   ~ 0+Intercept+group*condition*delta_exp_value+(0+Intercept+condition*delta_exp_value|subject),
  sigma ~ 0+Intercept)


#brms family argument
myfamily=exgaussian(link = "identity", link_sigma = "log", link_beta = "log")

get_prior(myformula, family = exg_family, data=df)

#brms prior argument (you can examine brms defualts using get_prior(myformula, family      = myfamily,data=df))
myprior  = c(
  
  #meanrt 
  set_prior(prior="normal(0.4,0.2)", class="b", coef="Intercept", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionchoice", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionoutcome", dpar=""),
  set_prior(prior="normal(0,0.3)", class="b", coef="group1:conditionchoice", dpar=""),
  set_prior(prior="normal(0,0.3)", class="b", coef="group1:conditionoutcome", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="delta_exp_value", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1:delta_exp_value", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionchoice:delta_exp_value", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionoutcome:delta_exp_value", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1:conditionchoice:delta_exp_value", dpar=""),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1:conditionoutcome:delta_exp_value", dpar=""),
  
  #sigma
  set_prior(prior="normal(-3, 0.5)", class="b", coef="Intercept", dpar="sigma"),
  
  #tau
  set_prior(prior="normal(-1.20,  0.5)", class="b", coef="Intercept", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionchoice", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionoutcome", dpar="tau"),
  set_prior(prior="normal(0,0.3)", class="b", coef="group1:conditionchoice", dpar="tau"),
  set_prior(prior="normal(0,0.3)", class="b", coef="group1:conditionoutcome", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="delta_exp_value", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1:delta_exp_value", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionchoice:delta_exp_value", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="conditionoutcome:delta_exp_value", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1:conditionchoice:delta_exp_value", dpar="tau"),
  set_prior(prior="normal(0,0.5)", class="b", coef="group1:conditionoutcome:delta_exp_value", dpar="tau")
  
)


#init argument 
myinit  <- list(b = c(1,0,0,0,0,0), 
                b_sigma = c(-3), 
                b_tau  = c(-1.2,0,0,0,0,0,0,0,0,0,0,0),
                sd_1    = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2),
                sd_2    = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2)
)

exg_stanvars <- exgaussian2_stancode()
exg_family <- exgaussian2()


fit <- brm(
  myformula,
  data = df,
  # brms.exgaussian additions
  family = exg_family,
  stanvars = exg_stanvars,
  # Sampler
  backend = "cmdstanr",
  iter = 8000, 
  warmup = 4000, 
  refresh = 0,
  cores = 4
)


describe_posterior(fit, ci=0.89,rope_range = c(-.5,.5))
conditional_effects(fit)





library(emmeans)
#emmeans(model,"1",regrid = "response", ci=0.89)

emmeans(fit, 
        myformula=brmsformula(
          rt    ~ 0+Intercept+diff_stage+(0+Intercept|subject),
          tau   ~ 0+Intercept+diff_stage+(0+Intercept|subject),
          sigma ~ 0+Intercept))
        
emmeans(fit, ~diff_stage,regrid = "response", dpar = "tau",at=list(diff_stage = c(0,0.5)), ci=0.89)