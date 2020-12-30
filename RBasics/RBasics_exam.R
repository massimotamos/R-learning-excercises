library(dslabs)
library(dplyr)
data(heights)
options(digits=3)
str(heights)
avg <-mean(heights$height)

higher_avg <- heights$height > avg

num_higher_avg <- sum(higher_avg)

num_higher_avg_f <- sum(higher_avg & heights$sex=="Female")

prop_female <- mean(heights$sex=="Female")

min_height_ind <- which.min(heights$height)

min_height <- heights$height[min_height_ind]

match_min_height_ind <- match(c(50),heights$height)

subset_sex <- as.character(heights$sex[match_min_height_ind])

max_height_ind <- which.max(heights$height)

max_height <- heights$height[which.max(heights$height)]

x <- heights$height[c(50:82)]

x<- 50:82

int_not_in_x <- sum(!(x%in%heights$height))

heights2 <- mutate(heights,ht_cm=height*2.54)

height_18 <- heights2$ht_cm[c(18)]

mean_cm <- mean(heights2$ht_cm)

females <- filter(heights2,sex=="Female")

num_females <- nrow(females)

mean_height_f_cm <- mean(females$ht_cm)

