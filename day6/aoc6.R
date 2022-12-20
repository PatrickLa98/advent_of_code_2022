rm(list = ls())
library(tidyverse)

## load in data
data <- read.table("~/Documents/GitHub/advent_of_code_2022/day6/day6_data.txt")
str_length(data$V1)
data <- readChar("~/Documents/GitHub/advent_of_code_2022/day6/day6_data.txt", nchars = 4095)

df <- data.frame("characters" = str_split(data, "")[[1]], identifier = 1:4095)



## PARTA

dfA <- df
marker <- data.frame()

for (i in 1:nrow(dfA)) {
  

potential_marker <- dfA %>% 
                    slice(i:(i+3)) %>%
                    distinct(characters) 


if(nrow(potential_marker) == 4) {
  
  temp <- potential_marker
  temp$end_point <- i + 3
  marker <- bind_rows(temp, marker)
}

}

## 1155


## PARTB

dfB <- df
marker <- data.frame()

for (i in 1:nrow(dfB)) {
  
  
  potential_marker <- dfB %>% 
    slice(i:(i+13)) %>%
    distinct(characters) 
  
  
  if(nrow(potential_marker) == 14) {
    
    temp <- potential_marker
    temp$end_point <- i + 13
    marker <- bind_rows(temp, marker)
  }
  
}

## 2789