library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


temp_carbon %>%
  ggplot(aes(year,carbon_emissions)) +
  geom_vline(aes(xintercept = 1960),col="blue") +
  geom_vline(aes(xintercept = 1970),col="green") +
  geom_vline(aes(xintercept = 2014),col="red") +
  geom_point() 


historic_co2

co2_time <-historic_co2 %>% filter(!is.na(year) & !is.na(co2)) %>%
  ggplot(aes(year,co2))  +  geom_line(aes(col=co2)) +
  scale_x_continuous(limit=c(-800000,-775000))
co2_time

co2_time <-historic_co2 %>% filter(!is.na(year) & !is.na(co2)) %>%
  ggplot(aes(year,co2))  +  geom_line(aes(col=co2)) +
  scale_x_continuous(limit=c(-3000,2018))
co2_time
