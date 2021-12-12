# Functions to calculate the various indices

# This is the main entry function, which calculated all indices for a specific
#  group/country/configuration
calc.indices <- function(country.data, summary.data, group, drop.cats = F, weights = F) {
  n.eff <- nrow(country.data)
  
  # Calculate some summary data used for many of the indices
  group.sizes <- summary.data %>% 
    group_by(Group) %>% 
    tally(n) %>%
    mutate(percent = n / sum(n))
  
  party.sizes <- summary.data %>% 
    group_by(Party) %>% 
    tally(n) %>%
    mutate(percent = n / sum(n))
  
  party.support.by.group <- summary.data %>%
    group_by(Party) %>%
    mutate(Party_Total = sum(n)) %>%
    mutate(percent = n / Party_Total)
  
  # TODO: handle n.eff < 200
  
  tau <- calc.tau(country.data, group, weights = weights)
  
  gallager  <- calc.gallagher.new(party.support.by.group, group.sizes, party.sizes)
  loosemore <- calc.gallagher.new(party.support.by.group, group.sizes, party.sizes, loosemore = T)
  
  return (tibble(
    group = group,
    n.eff = n.eff,
    tau = tau,
    gallager = gallager,
    loosemore = loosemore
  ))
  
}

calc.tau <- function(country.data, group, weights = F) {
  wt.var = NULL
  if (weights)
    wt.var = "Weight"
  
  country.data <- country.data %>% mutate(
    Party = fct_drop(Party),
    "{group}" = fct_drop(.data[[group]])
  )  
  
  assoc <- NULL
  try({
    assoc <- suppressWarnings(pw.assoc(as.formula(paste(group, "~ Party")), country.data, out.df = T, weights = wt.var))
  })
  
  assoc$tau
}

calc.gallagher.new <- function(party.sizes.by.grp, grp.sizes, party.sizes, loosemore = F) {
  # Calculate unweighted values for each party
  res <- party.sizes.by.grp %>% 
    inner_join(grp.sizes, by = "Group") %>%
    mutate(value = percent.x - percent.y)
  
  if (loosemore) {
    res <- res %>% 
      mutate(value = abs(value)) %>% 
      group_by(Party) %>% 
      summarise(total = sum(value)/2)
  }
  else {
    res <- res %>% 
      mutate(value = value*value) %>% 
      group_by(Party) %>% 
      summarise(total = sqrt(sum(value)/2))
  }
  
  # Apply weights
  res.wt <- party.sizes %>%
    inner_join(res, by = "Party") %>%
    mutate(total = total * percent)
  
  res.wt %>% 
    ungroup() %>% 
    summarise(total = sum(total)) %>%
    pull(total) * 100
}