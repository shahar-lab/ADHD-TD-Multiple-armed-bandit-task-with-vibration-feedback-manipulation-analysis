library(brms)
library(brms.exgaussian)

myformula=brmsformula(
  rt    ~ 0+Intercept+diff_stage+(0+Intercept|subject),
  tau   ~ 0+Intercept+diff_stage+(0+Intercept|subject),
  sigma ~ 0+Intercept)


#brms family argument
#myfamily=exgaussian(link = "identity", link_sigma = "log", link_beta = "log")


#brms prior argument (you can examine brms defualts using get_prior(myformula, family      = myfamily,data=df))
myprior  = c(
  
  #meanrt 
  set_prior(prior="normal(0.65,0.1)", class="b", coef="Intercept", dpar=""),
  
  #sigma
  set_prior(prior="normal(-3, 0.5)", class="b", coef="Intercept", dpar="sigma"),
  
  #tau
  set_prior(prior="normal(-1.20,  0.5)", class="b", coef="Intercept", dpar="tau")
  
)

#init argument 
myinit  <- list(b = c(1,0,0,0,0,0), 
                b_sigma = c(-3), 
                b_tau  = c(-1.2,0,0,0,0,0),
                sd_1    = c(0.2,0.2,0.2,0.2),
                sd_2    = c(0.2,0.2,0.2,0.2)
)

exg_stanvars <- exgaussian2_stancode()
exg_family <- exgaussian2()


fit <- brm(
  bf(
    rt ~ group,
    tau ~ group,
    sigma ~ group
  ),
  data = df,
  # brms.exgaussian additions
  family = exg_family,
  stanvars = exg_stanvars,
  # Sampler
  backend = "cmdstanr",
  iter = 4000, 
  warmup = 1000, 
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

