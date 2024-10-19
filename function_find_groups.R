# this function separates the column ranges by group depending on whether the it 
# contains character variables and then includes in the range the following columns
# containing number variables. This enables splitting the data up wrt the one-hot-encoding
# and simplifies the overview of the data in view of the research questions. 

find_groups = function(data) {
  group_names = data %>% 
    select(where(is.character)) %>% 
    names()
  
  group_ranges = list()
  for (i in seq_along(group_names)) {
    start = which(names(data) == group_names[i])    # which returns the index of the matching value
    if (i < length(group_names)) {
      next_start = which(names(data) == group_names[i + 1]) - 1  # goes until next matching char_col with i + 1, then subtracts 1 to end at the right numeric
      group_ranges[[group_names[i]]] = start:next_start
    } else {
      group_ranges[[group_names[i]]] = start:ncol(data) # handles the end case
    }
  }
  
  return(group_ranges)
}
