load('./data/regression/acc~delta_exp_value*condition*group.rdata')

conditions <- make_conditions(model, "condition")
conditional_effects(model, "delta_exp_value:group", conditions = conditions)
bayestestR::describe_posterior(model, ci=(.89))


model |>
  emmeans(~ group,
          epred = T,
          level = 0.89,
          re_formula = NA) |>
  
  contrast(list('group effect'=c(-1,+1)))

# emmeans:
em = emtrends(model, ~ delta_exp_value * condition * group, 
              var = c("delta_exp_value"), 
              at = list(condition = c('off','choice','outcome'), group = c('td','adhd')))

# ADHD:
cont1= emmeans::contrast(em, list('adhd_vibration_vs_off'=c(0,0,0,-1,0.5,0.5)))

cont1 |> gather_emmeans_draws() |> describe_posterior()
hpd.summary(cont1,0.89)

# TD:
cont2= emmeans::contrast(em, list('td_vibration_vs_off'=c(-1,0.5,0.5,0,0,0)))

cont2 |> gather_emmeans_draws() |> describe_posterior()
hpd.summary(cont2,0.89)

# 3-Way - ADHD vs TD:
cont3= emmeans::contrast(em, list('adhd_vs_td_vibration_vs_off'=c(1,-0.5,-0.5,-1,0.5,0.5)))

cont3 |> gather_emmeans_draws() |> describe_posterior()
hpd.summary(cont3,0.89)
