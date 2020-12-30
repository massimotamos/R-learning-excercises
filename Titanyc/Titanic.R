options(digits=3)
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  filter(!is.na(Age)) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

params %>% ggplot(aes(sample=mean)) + geom_qq() + geom_abline()

titanic <- titanic_train %>%
  filter(!is.na(Age)) %>% 
  ggplot(aes(Survived,Sex)) + geom_bar(stat="identity")
print(titanic)

 titanic_train %>%
  filter(!is.na(Age)) %>% 
   mutate(Survived = factor(Survived),
          Pclass = factor(Pclass),
          Sex = factor(Sex)) %>%
  ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha = 0.2)

titanic_train %>%
   filter(!is.na(Age) & !Fare==0) %>% 
   mutate(Survived = factor(Survived),
          Pclass = factor(Pclass),
          Sex = factor(Sex)) %>%
   ggplot(aes(Survived,Fare)) + geom_boxplot(alpha=0.2) + scale_y_continuous(trans = "log2") + 
  geom_point(show.legend = FALSE) + geom_jitter(width = 0.1, alpha = 0.2)


titanic_train %>%
  filter(!is.na(Age) & !Fare==0) %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>%
  ggplot(aes(Pclass,y=..count..,fill=Survived)) + geom_bar()

titanic_train %>%
  filter(!is.na(Age) & !Fare==0) %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>%
  ggplot(aes(Pclass,fill=Survived)) + geom_bar(position=position_fill())


titanic_train %>%
  filter(!is.na(Age) & !Fare==0) %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>%
  ggplot(aes(Survived,y=..count..,fill=Pclass)) + geom_bar(position=position_fill())


titanic_train %>%
  filter(!is.na(Age) & !Fare==0) %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>%
  ggplot(aes(Survived,fill=Pclass)) + geom_bar(position=position_fill())

titanic_train %>%
  filter(!is.na(Age) & !Fare==0) %>% 
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>%
  ggplot(aes(Age,y=..count..,fill=Survived)) +  geom_density(alhpa=0.2) + facet_wrap(Pclass~Sex)






  
 