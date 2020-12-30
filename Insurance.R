#Questions 5 and 6: Insurance rates, part 3

#Question 5a
#Calculate the premium required for a 5% chance of losing money given  ????=1000  loans, 
#probability of death  ????=0.015 , and loss per claim  ????=???150000 . Save this premium as x for use in further questions.
p_death<-0.015
p_loss<-0.05
n<-1000
l<--150000
z<-qnorm(p_loss)
x<--l*(n*p_death -z*sqrt(n*p_death*(1-p_death)))/(n*(1-p_death)+z*sqrt(n*p_death*(1-p_death)))
x

#Question 5b
#What is the expected profit per policy at this rate?
Ex_pl<-p_death*l+(1-p_death)*x
Ex_pl

Ex_pl_1k<-Ex_pl*10^3
Ex_pl_1k

#Question 5d
B<-10000
set.seed(29, sample.kind = "Rounding")
n<-1000
loss<--150000
p_loss<-0.015
prob_loss<-replicate(B,{
  pl<-sample(c(loss,x),n,replace = TRUE,prob=c(p_loss,1-p_loss))
  sum(pl)<=0
})
mean(prob_loss)


#Question 6c
B<-10000
set.seed(29, sample.kind = "Rounding")
n<-1000
loss<--150000
p_loss_init<-0.015
P_loss<-replicate(B,{
  p_loss_var <- sample(seq(-0.01, 0.01, length = 100),1)
  p_loss<-p_loss_init + p_loss_var
  pl<-sample(c(loss,x),n,replace = TRUE,prob=c(p_loss,1-p_loss))
  sum(pl)<=-10^6
})
mean(P_loss)
