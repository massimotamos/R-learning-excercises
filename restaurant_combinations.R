library(gtools)
library(tidyverse)
#phone_numbers<-permutations(2,7,v=0:9)
phone_numbers<-0
phone_numbers<-permutations(10,7)
phone_numbers

rna<-permutations(3,2,letters[1:3])
rna

medals<-0
medals<-permutations(8,3)
n<-nrow(medals)
n


medals<-0
medals<-permutations(3,3)
medals
n<-nrow(medals)
n



B<-10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
jamaica_win<- replicate(B, {
  run<- sample(runners,3)
  #print(run[1])
  #jamaica_win<-(run[1]=="Jamaica" & run[2]=="Jamaica" & run[3]=="Jamaica")
  jamaica_win<-all("Jamaica"==run)
})
#print(jamaica_win)
mean(jamaica_win)

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)
deck

entrees<-letters[1:6]
sides<-letters[1:6]
drinks<-as.character(1:2)
v<-paste(entrees,sides)
v
c_menus<-combinations(length(entrees)+length(sides)+length(drinks),3,v=paste(entrees,sides,drinks))
c_menus

types<-c("entree","side")
numbers<-as.character(1:6)
drinks<-as.character(1:2)
deck<-expand.grid(food=numbers,type=types)
deck
deck<-paste(deck$food,deck$type)
deck
all<-expand.grid(deck=deck,drink=drinks)
all
all<-paste(all$deck,all$drink)
all
length(all)

c_drinks<-combinations(3,1,v=1:3,repeats.allowed=TRUE)
nrow(c_drinks)

c_sides<-combinations(6,3,v=1:6,repeats.allowed=FALSE)
nrow(c_sides)

c_entree<-combinations(6,1,v=1:6,repeats.allowed=TRUE)
nrow(c_entree)

c_menus=nrow(c_drinks)*nrow(c_sides)*nrow(c_entree)
c_menus


get_c_entrees <- function(n){
  c_entree<-combinations(n,1,v=1:n,repeats.allowed=FALSE)
  n_entrees<-nrow(c_entree)
  c_drinks<-combinations(3,1,v=1:3,repeats.allowed=TRUE)
  n_drinks<-nrow(c_drinks)
  c_sides<-combinations(6,2,v=1:6,repeats.allowed=FALSE)
  n_sides<-nrow(c_sides)
  n_c_menus<-n_entrees*n_drinks*n_sides
}
sapply(1:12,get_c_entrees)

get_c_menus <- function(n){
  c_entree<-combinations(6,1,v=1:6,repeats.allowed=FALSE)
  n_entrees<-nrow(c_entree)
  c_drinks<-combinations(3,1,v=1:3,repeats.allowed=TRUE)
  n_drinks<-nrow(c_drinks)
  c_sides<-combinations(n,2,v=1:n,repeats.allowed=FALSE)
  n_sides<-nrow(c_sides)
  n_c_menus<-n_entrees*n_drinks*n_sides
}
sapply(2:12,get_c_menus)