# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
head(brexit_polls)

brexit_polls_1 <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2), d_hat= mean(spread), sd_d=sd(spread), sd_x_hat=sd((spread + 1)/2), se_x_hat=sqrt(x_hat*(1-x_hat)/4772))

head(brexit_polls_1)

first_poll <- brexit_polls[1,] %>%   mutate(x_hat = (spread + 1)/2), d_hat= mean(spread), sd_d=sd(spread), sd_x_hat=sd((spread + 1)/2), se_x_hat=sqrt(x_hat*(1-x_hat)/4772))
head(first_poll)
first_poll$x_hat
ci_low <- first_poll$x_hat - qnorm(.975)*first_poll$se_x_hat
ci_low
ci_up <- first_poll$x_hat + qnorm(.975)*first_poll$se_x_hat
ci_up

#-----

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize), se_d=(2*sqrt(x_hat*(1-x_hat)/samplesize)), ci_l=(-0.038 - qnorm(.975)*se_d),
         ci_u=(-0.038 + qnorm(.975)*se_d), hit=(between(-0.038,ci_l,ci_u)))
june_polls
N_pools_june<-sum(june_polls$samplesize)
N_pools_june
p_polls_remain <- mean(june_polls$ci_l >0)
p_polls_remain
p_polls_d <- mean(june_polls$hit)
p_polls_d

June_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), se_spread = 2*se_x_hat, lower = (spread-qnorm(0.975)*se_spread), upper=(spread+qnorm(0.975)*se_spread), hit=(lower<=???0.038 & upper>=???0.038))
June_polls
N_pools_june<-nrow(June_polls)

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), se_spread = 2*se_x_hat, lower = (spread-qnorm(0.975)*se_spread), upper=(spread+qnorm(0.975)*se_spread), hit=(???0.038>=lower & ???0.038<=upper))
june_polls
p_polls_zero <- mean(june_polls$lower <=0 & june_polls$upper >=0)
p_polls_zero
p_polls_above_zero <- mean(june_polls$lower >=0)
p_polls_above_zero
p_polls_within_d<-mean(june_polls$hit)
p_polls_within_d

hits_stats<-june_polls %>% group_by(pollster) %>% summarise(N=n(),p_hit_pollster=(sum(hit==TRUE))/N,
                                                            n_polls_polster=sum(samplesize)/N) %>% arrange(desc(p_hit_pollster))
hits_stats

head(brexit_polls)

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% ggplot(aes(spread,poll_type)) + geom_boxplot() + geom_point()
june_polls

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (sprea1-p_hat)/N),ci_d_l=spread - qnorm(0.975)*2*se,ci_d_u=spread + qnorm(0.975)*2*se)
combined_by_type %>% ggplot(data= c(ci_d_l,ci_d_u), aes(diff,poll_type)) + geom_line()d + 1)/2, se=sqrt(p_hat*(
combined_by_type

combined_by_type <- june_polls %>% group_by(poll_type) %>% summarize(N = sum(samplesize), spread = sum(spread*samplesize)/N, p_hat = ((spread + 1)/2), se = 2* sqrt(p_hat*(1-p_hat)/N), lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se)
combined_by_type

combined_by_type <- june_polls %>% group_by(poll_type) %>% summarize(N = sum(samplesize), spread = sum(spread*samplesize)/N, p_hat = ((spread + 1)/2), se = 2* sqrt(p_hat*(1-p_hat)/N), lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se)
combined_by_type

#-------------------------
# Question 9: Chi-squared p-value
#-------------------------

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
t_pool <-brexit_hit %>% group_by(poll_type,hit) %>% summarize(hit_rate=n())  %>% spread(poll_type,hit_rate)
t_pool
chisq_test <- t_pool %>% select(-hit) %>% chisq.test()
chisq_test$p.value

# final proportion voting "Remain"
p <- 0.481

brexit_diag <- brexit_polls %>% ggplot(aes(enddate, spread,color=poll_type)) + geom_smooth(method="loess", span=0.4) + geom_point() + geom_hline(yintercept=-0.038)
brexit_diag


brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(enddate,proportion,color=vote)) + geom_smooth(method="loess", span=0.3)
