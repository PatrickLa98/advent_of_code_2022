## NOTE: TO SOLVE PARTA REMOVE "rev()" in line 75


library(tidyverse)

data <- readLines("~/Documents/GitHub/advent_of_code_2022/day5/day5_data.txt")

## split to stacks and procedure
stacks <- data[1:8]
procedure <- data[11:512]

stacks_list <- str_split(stacks, "") 

## remove the weird brackets around letters
stacks_list <- lapply(stacks_list, function(x) {
  gsub(" |\\]|\\[", "", x) }) 


## get position with gap info
position_list <- data[9] %>% 
  str_split("")


## format procedure
procedure_extracted  <-strsplit(procedure, split = "(move|from|to)")
procedure_df <- data.frame()

for (j in 1:length(procedure)) {
  
amount = as.integer(procedure_extracted[[j]][2])
original_hor_pos = as.integer(procedure_extracted[[j]][3])
new_hor_pos = as.integer(procedure_extracted[[j]][4])

temp <- data.frame(amount = amount, original_hor_pos = original_hor_pos, new_hor_pos = new_hor_pos)

procedure_df <- rbind(procedure_df, temp)                  

}


## process stacks to beautiful data frame
stacks_df <- data.frame()

for(i in 1: length(stacks)) {
  
  stacks_vector <- stacks_list[[i]][which(stacks_list[[i]] %in% LETTERS)]
  stacks_position_with_gaps <- which(stacks_list[[i]] %in% LETTERS)
  position_horizontal <-as.numeric(position_list[[1]][stacks_position_with_gaps])
  
  temp <- data.frame(stack = stacks_vector, position_vertical = i, position_horizontal = as.integer(position_horizontal))
  
  stacks_df <- rbind(stacks_df,temp)
  
}

## flip vertical positions so that stacks on top become max value
stacks_df <- stacks_df %>% 
  group_by(position_horizontal) %>% 
  mutate(position_vertical = rev(position_vertical)) %>%
  ungroup() %>% 
  mutate(identifier = 1:nrow(stacks_df))

## follow the described procedure in data input
for (k in 1:nrow(procedure_df)) {
  
  #only add amount on top of the new stack if new stack already has other stacks otherwise set to 0
  if(!is.na(stacks_df$position_vertical[stacks_df$position_horizontal == procedure_df$new_hor_pos[k]][1])) {
  new_stack_max_vertical_position <- max(stacks_df$position_vertical[stacks_df$position_horizontal == procedure_df$new_hor_pos[k]])
  
  } else {
    new_stack_max_vertical_position <- 0
  }
    
add_amount_to_new_stack <- 1: procedure_df$amount[k]
  new_pos_ver <- rev(new_stack_max_vertical_position  + add_amount_to_new_stack) #to solve PARTA remove rev
  
 new_pos <- stacks_df %>% 
    filter(position_horizontal == procedure_df$original_hor_pos[k]) %>% 
   arrange(desc(position_vertical)) %>% 
    head(procedure_df$amount[k]) %>% 
    mutate(position_horizontal = as.integer(procedure_df$new_hor_pos[k])) %>% 
   mutate(position_vertical = new_pos_ver[1:nrow(.)])

 ## replace old position information with updated one
 for (i in 1:nrow(new_pos)) {
   
   stacks_df$position_horizontal[stacks_df$identifier == new_pos$identifier[i]] <- new_pos$position_horizontal[i]
   stacks_df$position_vertical[stacks_df$identifier == new_pos$identifier[i]] <- new_pos$position_vertical[i]
   
 }

}

## results
stacks_df %>% 
  group_by(position_horizontal) %>% 
  arrange(desc(position_vertical)) %>% 
  slice(1)

#PARTA: SPFMVDTZT
#PARTB: ZFSJBPRFP



