library(tidyverse)

df <- read_table("~/Desktop/Sonstiges/Advent of Code/day2/day2_data.txt")


## PARTA
dfA <- df

dfA$points_choice <- NA
dfA$points_choice[which(dfA$response == "X")] <- 1
dfA$points_choice[which(dfA$response == "Y")] <- 2
dfA$points_choice[which(dfA$response == "Z")] <- 3

dfA$points_outcome <- NA

for (i in 1:nrow(dfA)) {
  
  if(dfA$opponent[i] == "A" & dfA$response[i] == "Y" |
     dfA$opponent[i] == "B" & dfA$response[i] == "Z" |
     dfA$opponent[i] == "C" & dfA$response[i] == "X"
     ) {
    
    dfA$points_outcome[i] <- 6
    
  } else if (dfA$opponent[i] == "A" & dfA$response[i] == "X" |
    dfA$opponent[i] == "B" & dfA$response[i] == "Y" |
    dfA$opponent[i] == "C" & dfA$response[i] == "Z"
  ) {
    
    dfA$points_outcome[i] <- 3
    
  } else {
    
    dfA$points_outcome[i] <- 0
    
    
  }
  
    
}

dfA$total_points <- dfA$points_choice + dfA$points_outcome

sum(dfA$total_points)



## PARTB

dfB <- df |>
  rename(outcome = response) 

dfB$play <- NA


for (i in 1:nrow(dfB)) {
  
  if(dfB$opponent[i] == "A" & dfB$outcome[i] == "Y" | # rock - draw
     dfB$opponent[i] == "B" & dfB$outcome[i] == "X" | # paper - loose
     dfB$opponent[i] == "C" & dfB$outcome[i] == "Z" # sissors - win
  ) {
    
    dfB$play[i] <- 1 # rock
    
  } else if (dfB$opponent[i] == "A" & dfB$outcome[i] == "Z" | # rock - win
             dfB$opponent[i] == "B" & dfB$outcome[i] == "Y" | # paper -draw
             dfB$opponent[i] == "C" & dfB$outcome[i] == "X"   # sissors -loose
  ) {
    
    dfB$play[i] <- 2 # paper
    
  } else {
    
dfB$play[i] <- 3 # sissors    
    
  }
  
  
}

dfB$outcome_points <- NA

dfB$outcome_points[which(dfB$outcome == "X")] <- 0
dfB$outcome_points[which(dfB$outcome == "Y")] <- 3
dfB$outcome_points[which(dfB$outcome == "Z")] <- 6


dfB$totalpoints <- dfB$play + dfB$outcome_points

sum(dfB$totalpoints)
