library(tidyr)
library(dplyr)

filtered_df = df|>pivot_longer(cols = c('exp_value1','exp_value2','exp_value3','exp_value4'),names_to = 'card',values_to = 'exp_value')|>
  select(card,exp_value,randomwalk_counter,trial)

ggplot(data = filtered_df|> filter(randomwalk_counter == 2), aes(x = trial, y = exp_value, color = card)) + geom_point() + geom_line()

