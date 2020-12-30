set.seed(21, sample.kind = "Rounding")
B<-10000

S<-replicate(B,{
  exam_result<-sample(c(1,-0.25),44,replace = TRUE,prob=c(1/5,4/5))
  sum(exam_result)
})
mean(S>8)

#-------

p<-seq(0.25,0.95,0.05)
p
ex<-44*(1*1/4-0*3/4)
ex
se<-sqrt(44)*(1-0)*sqrt(1/4*3/4)
se
p_30<-1-pnorm(30,ex,se)
quantile(p_30,p)

set.seed(21, sample.kind = "Rounding")
fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
}

p<-seq(0.25,0.95,0.05)
p
sapply(p, FUN=fu)


6*(5/38)+(-1)*(1-5/38)

avg<-(6*5/38 + -1*(1-5/38))

abs(6--1)*sqrt(5/38*(1-5/38))/sqrt(500)

se<-abs(6--1)*sqrt(5/38*(1-5/38))

e_val_sum<-500*(6*(5/38)+(-1)*(1-5/38))
se_sum<-sqrt(500)*abs(6--1)*sqrt(5/38*(1-5/38))
pnorm(0,e_val_sum,se_sum)
