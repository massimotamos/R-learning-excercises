library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)

stars %>% filter(!is.na(magnitude)) %>%
  ggplot(aes(magnitude)) + 
  geom_density()


stars %>% filter(!is.na(magnitude)) %>%
  ggplot(aes(temp)) + 
  geom_density()



stars %>% filter(!is.na(magnitude) & temp <=6000) %>%
  ggplot(aes(temp,magnitude)) + 
  geom_point() + scale_y_reverse() + scale_x_log10() + scale_x_reverse()

stars %>% filter(!is.na(temp) & temp >=5000 & temp <=5500) %>%
  ggplot(aes(temp,magnitude,col=type)) + 
  geom_point() + geom_text_repel(aes(label = star))


stars %>% filter(!is.na(temp) & temp<=6000) %>%
  ggplot(aes(temp,magnitude,col=type)) + 
  geom_point() + geom_text_repel(aes(label = star))

stars %>% filter(!is.na(temp) & star %in% c("Antares","Mirfak","Castor","Polaris","van Maanen's")) %>%
  ggplot(aes(temp,magnitude,col=type)) + 
  geom_point() + geom_text_repel(aes(label = star))



stars %>%
  ggplot(aes(log10(temp), magnitude,col=type)) +
  geom_point() +
  geom_text_repel(aes(label = star)) 

stars %>%  filter(!is.na(temp) & temp<=3000) %>%
  ggplot(aes(temp, magnitude,col=type)) +
  geom_point() +
  geom_text_repel(aes(label = star)) 

stars %>%  filter(!is.na(temp) & temp>=20000) %>%
  ggplot(aes(temp, magnitude,col=type)) +
  geom_point() +
  geom_text_repel(aes(label = star)) 

stars %>%
  filter(!is.na(temp) & star %in% c("Sun")) %>%
  ggplot(aes(log10(temp), magnitude,col=type)) +
  geom_point() +
  #geom_text_repel(aes(label = star)) +
  scale_x_reverse() +
  scale_y_reverse()