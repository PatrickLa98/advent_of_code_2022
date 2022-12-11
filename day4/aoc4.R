library(tidyverse)

df<- read.csv("~/Documents/GitHub/advent_of_code_2022/day4/day4_data.csv")

## PartA

dfA<- df %>%
  separate(elve1, into = c("start_section_elve1", "end_section_elve1")) %>% 
  separate(elve2, into = c("start_section_elve2", "end_section_elve2")) %>% 
  mutate_all(as.numeric)

dfA$fullycontained <- NA

for (i in 1:nrow(dfA)) {
  
sec_elve1 <- dfA$start_section_elve1[i]: dfA$end_section_elve1[i]
sec_elve2 <- dfA$start_section_elve2[i]: dfA$end_section_elve2[i]

sec_overlap1 <- sec_elve1 %in% sec_elve2
sec_overlap2 <- sec_elve2 %in% sec_elve1

if(all(sec_overlap1 == TRUE) | all(sec_overlap2 == TRUE)) {
  
  dfA$fullycontained[i] <- 1
} else {
  dfA$fullycontained[i] <- 0
  
}

}

sum(dfA$fullycontained)


## PartB

dfB<- df %>%
  separate(elve1, into = c("start_section_elve1", "end_section_elve1")) %>% 
  separate(elve2, into = c("start_section_elve2", "end_section_elve2")) %>% 
  mutate_all(as.numeric)

dfB$partlycontained <- NA

for (i in 1:nrow(dfB)) {
  
  sec_elve1 <- dfB$start_section_elve1[i]: dfB$end_section_elve1[i]
  sec_elve2 <- dfB$start_section_elve2[i]: dfB$end_section_elve2[i]
  
  sec_overlap1 <- sec_elve1 %in% sec_elve2
  sec_overlap2 <- sec_elve2 %in% sec_elve1
  
  if(any(sec_overlap1 == TRUE) | any(sec_overlap2 == TRUE)) {
    
    dfB$partlycontained[i] <- 1
  } else {
    dfB$partlycontained[i] <- 0
    
  }
  
}

sum(dfB$partlycontained)
