library(readxl)
library(tidyverse)

calories <- read_excel("~/Desktop/Sonstiges/Advent of Code/day1/aoc1.xlsx")

calories$elve <- NA

j <-1

for (i in 1:nrow(calories)) {
  
  if(is.na(calories$calories[i])) {
    
    j <- j +1
    
  } else {
    
    calories$elve[i] <- j
  } 
}


## PART A
 resulta <- calories %>% 
  group_by(elve) %>% 
  summarise(calories_per_elve = sum(calories)) %>% 
   arrange(desc(calories_per_elve)) 
 
head(resulta)
  
 
## PART B
 resulta %>% 
   head(3) %>% 
   summarise(totcal = sum(calories_per_elve))
