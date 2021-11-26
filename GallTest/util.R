gen.party.by.grp.df <- function(split) {
  size1 <- split
  size2 <- 100 - split
  
  l = list(
    c(
      "Party 1",
      "Group 1",
      size1
    ),
    c(
      "Party 1",
      "Group 2",
      size2 
    ),
    c(
      "Party 2",
      "Group 1",
      size2
    ),
    c(
      "Party 2",
      "Group 2",
      size1
    )
  )
  
  bind_rows(map(l, ~set_names(.x, c("Party", "Group", "val")))) %>%
    mutate(val = as.numeric(val))
}

calc.test.gall <- function(party.by.grp) {
  if (nrow(party.by.grp) == 0)
    return (0)
  
  total.votes <- sum(party.by.grp$val)
  
  if (total.votes <= 0)
    return (0)
  
  party.sizes <- party.by.grp %>% group_by(Party) %>% summarise(percent = sum(val) / total.votes)
  group.sizes <- party.by.grp %>% group_by(Group) %>% summarise(percent = sum(val) / total.votes) %>%
    rename(group = Group)
  
  party.by.grp.mod <- party.by.grp %>%
    group_by(Party) %>%
    mutate(percent = val / sum(val)) %>% 
    select(-val) %>% 
    pivot_wider(names_from = Group, values_from = percent)
  
  gallagher.impl(party.by.grp.mod, group.sizes, party.sizes)
}