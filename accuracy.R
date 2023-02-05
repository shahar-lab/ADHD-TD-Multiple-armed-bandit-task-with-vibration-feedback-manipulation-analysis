rm(list=ls())
#source('./functions/my_packages.R')
load('./data/empirical_data/df.rdata')

library(dplyr)
df = 
df |>
  group_by(trial,condition) |>
  summarise(acc = mean(accuracy))

library(ggplot2)

ggplot(df, aes(x = trial, y=acc,color=condition))+geom_point()+geom_line()
