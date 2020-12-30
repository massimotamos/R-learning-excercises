library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
policy_lump<- -150000
policy_premium<-1150 
prob_death_50_F<-death_prob %>% filter(sex=="Female" & age==50) %>% pull(prob)

exp_val<-(prob_death_50_F*policy_lump + (1-prob_death_50_F)*policy_premium)


se<-abs(policy_lump-policy_premium)*sqrt(prob_death_50_F*(1-prob_death_50_F))

exp_val_1k_p<-1000*exp_val
exp_val_1k_p

se_1k_p<-sqrt(1000)*se
se_1k_p

pnorm(0,exp_val_1k_p,se_1k_p)

prob_death_50_M<-death_prob %>% filter(sex=="Male" & age==50) %>% pull(prob)
prob_death_50_M


#Ex<-n*(a*p+b*(1-p))
Ex<-700000
n<-1000
p<-prob_death_50_M
a<- -150000
b<-(Ex/n-a*p)*(1/(1-p))

----------
  
sqrt(1000)*abs(policy_lump-1459.262)*sqrt(0.005013*(1-0.005013))

policy_lump<- -150000
policy_premium<-1459.262 
prob_death_50_M<-death_prob %>% filter(sex=="Male" & age==50) %>% pull(prob)
exp_val<-(prob_death_50_M*policy_lump + (1-prob_death_50_M)*policy_premium)
exp_val_1k_p
se<-abs(policy_lump-policy_premium)*sqrt(prob_death_50_M*(1-prob_death_50_M))
se_1k_p<-sqrt(1000)*se
se_1k_p
pnorm(0,exp_val_1k_p,se_1k_p)

------------
  
p_death<-0.015
n<-1000
loss<- -150000
p_premium<-1150
Ex_1k<-n*(loss*p_death + p_premium*(1-p_death))
Ex_1k
se<-abs(loss-p_premium)*sqrt(p_death*(1-p_death))
se_1k<-sqrt(n)*se
se_1k

pnorm(-10^6,Ex_1k,se_1k)

total_loss<- -10^6
p <- p <- seq(.01, .03, .0025)
fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 10^3 * (loss*p + p_premium*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(10^3) * abs(loss - p_premium) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  pnorm(total_loss, expected_value, standard_error)
}
p
sapply(p, FUN=fu)
-----------------------
  
#Question 4a
B<-10000
set.seed(27, sample.kind = "Rounding")
n<-1000
loss_amnt<- -150000
premium_amt<-1150
p_loss<-0.015
prob_pl<- replicate(B, {
   p_l<-sample(c(loss_amnt,premium_amt),n,replace = TRUE,prob=c(p_loss,1-p_loss))
   #sum(((p_l)/10^6)>=1)
   sum((p_l)/10^6)<=-1
})
mean(prob_pl)













