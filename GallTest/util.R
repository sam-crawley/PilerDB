gen.party.by.grp.df <- function(split) {
  size1 <- split
  size2 <- 100 - split
  
  tribble(
    ~Party,    ~Group,    ~n,
    "Party 1", "Group 1", size1,
    "Party 1", "Group 2", size2,
    "Party 2", "Group 1", size2,
    "Party 2", "Group 2", size1,    
  )
}