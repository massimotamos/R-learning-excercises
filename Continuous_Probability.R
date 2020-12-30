set.seed(16,sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
avg<-mean(act_scores)
std_dev<-sd(act_scores)
sum(act_scores>=36)

mean(act_scores<=10)

x<-seq(1:36)
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x,type="l")

z_scores<-((act_scores-avg)/std_dev)
mean(z_scores>2)

n2_std_scores<-avg+2*std_dev
n2_std_scores

perc_975<-qnorm(0.975,20.840,5.675)
perc_975

x<-seq(1:36)
CDF<- function(a) mean(x<=a)
sapply(x,CDF)

ceiling(qnorm(0.95,mean(act_scores),sd(act_scores)))

qnorm(0.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores,p)
sample_quantiles

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles<-qnorm(p,20.9,5.7)
qqplot(theoretical_quantiles,sample_quantiles)
qqline(theoretical_quantiles, col = "steelblue", lwd = 2)
