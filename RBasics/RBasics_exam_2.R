library(dslabs)
data(olive)
head(olive)


plot(olive$palmitic,olive$palmitoleic)

#perc_eico <- olive

x<-olive$eicosenoic/total_oil*100

hist(olive$eicosenoic)

boxplot(olive$palmitic~olive$region)
