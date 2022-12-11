
library(tidyverse)

df <- read_table("~/Documents/GitHub/advent_of_code_2022/day3/day3_data.txt")

#PARTA

## count items in bag
 df$n_items_in_bag <- str_length(df$items)

 ## separte items into the two compartments 
 df_compartments <- data.frame()
 items_sep1 <- NA
 items_sep2 <- NA
 
 for (i in 1: nrow(df)) {
   
   if(i == 1) {
     
     items_sep1 <- str_sub(df$items[i], start = 1, end = df$n_items_in_bag[i] / 2)
     items_sep2 <- str_sub(df$items[i], start = 1 + (df$n_items_in_bag[i] / 2), end = df$n_items_in_bag[i])
     
     rucksack <- i
     n_items = df$n_items_in_bag[i]/2
     
     
   } else {
   
  temp1  <-  str_sub(df$items[i], start = 1, end = df$n_items_in_bag[i] / 2)
  temp2 <-   str_sub(df$items[i], start = 1 + (df$n_items_in_bag[i] / 2), end = df$n_items_in_bag[i])
  
  
  items_sep1 <- c(items_sep1, temp1)
  items_sep2 <- c(items_sep2, temp2)
  
  rucksack <- c(rucksack, i)
  n_items = c(n_items, df$n_items_in_bag[i]/2)
  
   }
 }

 df_compartments <- data.frame(items_comp1 = items_sep1, items_comp2 = items_sep2, rucksack = rucksack, n_items = n_items )

## identify the item shared in the two compartments
 
 df_compartments$item_shared <- NA
 
for (i in seq(df_compartments$rucksack)) {
 
  for (j in 1: df_compartments$n_items[i]) {
    
    potential_match <-  str_sub(df_compartments$items_comp2[i], start = j, end = j)
    
      if(!is.na(str_match(df_compartments$items_comp1[i], potential_match))) {
        df_compartments$item_shared[i] <- str_match(df_compartments$items_comp1[i], potential_match)
      }
    
  }
} 
 
## add priorities to different items
 df_priorities <- data.frame(letter = c(letters, LETTERS), priority = c(1:52))
resultA <- inner_join(df_compartments, df_priorities, by = c("item_shared" ="letter"))

## result
sum(resultA$priority)


# PARTB

rm(list = ls())
df <- read_table("~/Documents/GitHub/advent_of_code_2022/day3/day3_data.txt")

## create group identifier
n_groups <- nrow(df) / 3
group <- rep(1, 3)

for (i in 2: n_groups) {
  
temp<- rep(i,3)  
group <- c(group, temp)

}

df$group <- group 

## identify different elves within group
df$elve_in_group <- rep(c("elve1", "elve2", "elve3"), n_groups)

df_by_group <- df %>% 
  group_by(group) %>% 
  pivot_wider(names_from = elve_in_group, values_from = items )

## find matching item between 3 elves in a group
for (j in 1: n_groups) {
split_elve2 <- str_split(df_by_group$elve2[j], "")[[1]]

match_elve1_elve2  <- str_match(df_by_group$elve1[j], split_elve2)

match <- str_match(df_by_group$elve3[j], match_elve1_elve2)
df_by_group$match[j] <- match[!is.na(match)]

}

## add priorities to items
df_priorities <- data.frame(letter = c(letters, LETTERS), priority = c(1:52))

resultB <- inner_join(df_by_group, df_priorities, by = c("match" ="letter"))

sum(resultB$priority)


