library(tidyverse)
library(dslabs)


temp_carbon %>% filter(!is.na(carbon_emissions))%>% .$year %>% max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()


y<-temp_carbon %>%
  .$year %>%
  max()

temp_carbon %>% filter(!is.na(carbon_emissions) & year==max(year)) %>% .$carbon_emissions

emission_2014<-temp_carbon %>% filter(!is.na(carbon_emissions) & year==2014) %>% .$carbon_emissions
emission_1751<-temp_carbon %>% filter(!is.na(carbon_emissions) & year==1751) %>% .$carbon_emissions 
increase_em<-emission_2014/emission_1751

temp_carbon %>% filter(!is.na(temp_anomaly))%>% .$year %>% min()
temp_carbon %>% filter(!is.na(temp_anomaly))%>% .$year %>% max()

init_temp<-temp_carbon %>% filter(!is.na(temp_anomaly) & year==1880) %>% .$temp_anomaly
last_temp<-temp_carbon %>% filter(!is.na(temp_anomaly) & year==2018) %>% .$temp_anomaly
delta_temp<-last_temp-init_temp

p<-temp_carbon %>% filter(!is.na(temp_anomaly))%>% ggplot(aes(year,temp_anomaly)) + geom_line()
p<-p + ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")
p<-p + geom_text(aes(x=2000, y=0.05, label="Temperature anomaly (degrees C)"), col = "blue") 
p


p<-temp_carbon %>% filter(!is.na(temp_anomaly))%>% ggplot(aes(year,temp_anomaly)) + geom_line()
p<-p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean", col = "blue"))
p


p<-temp_carbon %>% filter(!is.na(temp_anomaly))%>% ggplot(aes(year,temp_anomaly)) + geom_line()
p<-p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p


p<-temp_carbon %>% filter(!is.na(temp_anomaly))%>% ggplot(aes(year,temp_anomaly)) + geom_line()
p<-p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p<- p + geom_line(aes(year,ocean_anomaly,col="ocean"),label="ocean")
p<- p + geom_line(aes(year,land_anomaly,col="land"),label="land")
p

