rm(list=ls())

df<- read.csv('./data/empirical_data/df_raw.csv')
bio = read.csv('./data/empirical_data/bio.csv')
bio = bio|>mutate(ICAR =str_sub(ICAR,1,-2))
icar = as.numeric(bio[1:43,'ICAR'])

library(dplyr)

df <-df %>% filter(block_type==' test')

df= df %>% mutate(stay = (choice_card == lag(choice_card))*1, 
                  reward_oneback = lag(reward),
                  reoffer_chosen_oneback = (lag(choice_card) == left_offer | lag(choice_card) ==right_offer),
                  exp_value_unchosen = ifelse(exp_value_chosen == exp_value_left, exp_value_right,exp_value_left),
                  accuracy = (exp_value_chosen > exp_value_unchosen)*1,
                  stay_key = (choice_key == lag(choice_key))*1,
                  delta_exp_value = abs(exp_value_chosen - exp_value_unchosen),
                  condition = factor(condition),
                  condition_oneback =lag(condition),
                  group = factor(group),
                  block_phase = (trial <= 25)*0,
                  block_phase = (trial > 25)*1,
                  block_phase = factor(block_phase,levels = c(0,1), labels = c('first_half','second_half')),
                  delta_level = (delta_exp_value <= 0.15)*0,
                  delta_level = (delta_exp_value > 0.15)*1,
                  delta_level = factor(delta_level,levels = c(0,1), labels = c('hard','easy'))
)

df= df %>% mutate(abort = ( rt < 0.2 | rt > 4 | is.na(rt)))

#######omit subjects with more the 90% stay for key ----------------------

df%>%
  group_by(subject)%>%
  summarise(p.stay=mean(stay_key))%>%
  with(plot(p.stay,ylim=c(0,1)))

print('no subjects were ommited, all had pstay_key<0.9')


#######omit subjects with more the 25% trials aborted due to very long, very short RTs and first trial----------------------

p.abort=df%>%group_by(subject)%>%summarise(p.abort=mean(abort))%>%mutate(above_threshold=(p.abort>.25))
sum(p.abort$above_threshold)

#subj.list=p.abort$subject[p.abort$above_threshold==0]
#df=df%>%filter(subject%in%subj.list)

print('we ommited 0 participants since they had more then 25% trial ommited due to fast/slow RTs')


#######filter ----------------------

mean(df$abort)
print('we then ommited 2% of trials due to fast/slow rts or first trial in the block')

df=df%>%filter(abort==0)

df<-na.omit(df)


df= df %>% mutate(reoffer_unchosen_oneback = (lag(unchosen_card) == left_offer | lag(unchosen_card) ==right_offer),
                  stay_unchosen = lag(unchosen_card) == choice_card)

save(df, file='./data/empirical_data/df.rdata')

