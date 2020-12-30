library(dslabs)
data(heights)
#sum(ifelse(heights$sex=='Female',1,2))
#mean(ifelse(heights$height>72,heights$height,0))

inches_to_feet <- function(x) {
  x/12
}
print(inches_to_feet(144))

x<-sum(ifelse(heights$height<5*12,1,0))