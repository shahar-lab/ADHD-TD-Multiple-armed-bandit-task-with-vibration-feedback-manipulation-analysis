load('./data/regression/acc~1+group.rdata')

conditional_effects(model)
bayestestR::describe_posterior(model, ci=(.89))


model |>
  emmeans(~ group,
          epred = T,
          level = 0.89,
          re_formula = NA) |>
  
  contrast(list('group effect'=c(-1,+1)))

