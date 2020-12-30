library(gtools)
library(tidyverse)
head(esoph)
nrow(esoph)
all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)

p_high_alcohol <- esoph %>% filter(alcgp==max(alcgp)) %>%summarise(n_cancers=sum(esoph$ncases))


n_control_max_alcohol <- esoph %>% filter(alcgp==max(alcgp)) %>% .$ncontrol %>%sum()
n_cancer_max_alcohol <- esoph %>% filter(alcgp==max(alcgp)) %>% .$ncases %>%sum()
p_high_alcohol<-n_cancer_max_alcohol/(n_control_max_alcohol+n_cancer_max_alcohol)

n_control_min_alcohol <- esoph %>% filter(alcgp==min(alcgp)) %>% .$ncontrol %>%sum()
n_cancer_min_alcohol <- esoph %>% filter(alcgp==min(alcgp)) %>% .$ncases %>%sum()
p_low_alcohol<-n_cancer_min_alcohol/(n_control_min_alcohol+n_cancer_min_alcohol)


n_canc_smoke_no_min <- esoph %>% filter(!tobgp==min(tobgp)) %>% .$ncases %>% sum()

p_canc_smoke_no_min <-n_canc_smoke_no_min/sum(esoph$ncases)

n_ctrl_smoke_no_min <- esoph %>% filter(!tobgp==min(tobgp)) %>% .$ncontrols %>% sum()

p_ctrl_smoke_no_min <-n_canc_smoke_no_min/sum(esoph$ncontrols)

n_cancer_max_tob <- esoph %>% filter(tobgp==max(tobgp)) %>% .$ncases %>%sum()
n_cancer_max_tob

esoph %>% filter(alcgp == "120+" ) %>%
  summarize(sum_cases = sum(ncases))

esoph %>% filter(tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))

esoph %>% filter(alcgp == "120+" | tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))

esoph %>% filter(tobgp =="30+") %>%
  summarize(sum_ctrl = sum(ncontrols))


esoph %>% filter(alcgp == "120+" & tobgp =="30+") %>%
  summarize(sum_ctrl = sum(ncontrols))
