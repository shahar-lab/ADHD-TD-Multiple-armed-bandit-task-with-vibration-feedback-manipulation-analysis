load('./data/regression/acc~1+group.rdata')

describe_posterior(model)

model |>
  emmeans(~ group,
          epred = T,
          level = 0.89,
          re_formula = NA) |>
  
  contrast(list('group effect '=c(-1,+1)))

