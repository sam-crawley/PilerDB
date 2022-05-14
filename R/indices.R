# Functions to calculate the various indices

# This is the main entry function, which calculated all indices for a specific
#  group/country/configuration
calc.indices <- function(country.data, summary.data, group, drop.cats = F, weighted = F) {
  summary.data <- config.summary.data(summary.data, drop.cats = drop.cats, weighted = weighted)
  
  if (weighted) {
    # Make sure weights are valid - drop any rows out if they are missing weights
    country.data <- country.data %>% filter(! is.na(Weight))
  }
  
  if (drop.cats & ! is.null(country.data))
    country.data <- drop.rows.from.country.data(country.data, group, weighted = weighted)

  n.eff <- nrow(country.data)
  
  if (! is.data.frame(summary.data) || n.eff <= 200)
    return(
      tibble(
        group = group,
        n.eff = n.eff,
        tau = NA
      )
    )  
  
  tau <- calc.tau(country.data, group, weighted = weighted)
  
  summary.indices <- calc.summary.indices(summary.data)
  
  res <- tibble(
    group = group,
    n.eff = as.integer(n.eff),
    parties = as.integer(length(unique(summary.data$Party))),
    groups = as.integer(length(unique(summary.data$Group))),
    tau = tau
  )
  
  bind_cols(res, summary.indices)
  
  
}

calc.summary.indices <- function(summary.data, include.extra = T) {
  index.summaries <- build.index.summary.data(summary.data)

  gallagher <- calc.gallagher(index.summaries$party.support.by.group, index.summaries$group.sizes, index.summaries$party.sizes)
  loosmore <- calc.gallagher(index.summaries$party.support.by.group, index.summaries$group.sizes, index.summaries$party.sizes, loosmore = T)
  huber <- calc.huber.indices(summary.data, 
                              index.summaries$group.sizes, index.summaries$party.sizes, index.summaries$group.size.by.party, index.summaries$party.support.by.group)
  
  res <- tibble(
    gallagher = gallagher,
    loosmore = loosmore
  )
  
  if (include.extra) {
    res$gatev <- calc.gatev(index.summaries$party.support.by.group, index.summaries$group.sizes, index.summaries$party.sizes, wt.by.party = T)
    res$gatev.no.wt <- calc.gatev(index.summaries$party.support.by.group, index.summaries$group.sizes, index.summaries$party.sizes, wt.by.party = F)
  }
  
  bind_cols(res, huber)
}

# Update the summary indices (i.e. not tau, since it requires the full data) for a 
#  given configuration
update.summary.indices <- function(orig.indices, summary.data.list, drop.cats = F, weighted = F) {
  orig.indices <- orig.indices %>% select(any_of(c("group", "n.eff", "parties", "groups", "tau")))
  
  new.indices <- map_dfr(names(summary.data.list), function(group) {
    summary.data <- config.summary.data(summary.data.list[[group]], drop.cats = drop.cats, weighted = weighted)
    
    res <- tibble(
      group = group
    )
    
    if (! is.data.frame(summary.data))
      return (res)
  
    bind_cols(res, calc.summary.indices(summary.data))
  })
  
  left_join(orig.indices, new.indices, by = "group")
}

# Calculate some summary data used for many of the indices
build.index.summary.data <- function(summary.data) {
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
  
  group.size.by.party <- summary.data %>%
    group_by(Group) %>%
    mutate(Group_Total = sum(n)) %>%
    mutate(percent = n / Group_Total)
  
  return (list(
    group.sizes = group.sizes,
    party.sizes = party.sizes,
    party.support.by.group = party.support.by.group,
    group.size.by.party = group.size.by.party
  ))
}

calc.tau <- function(country.data, group, weighted = F) {
  wt.var = NULL
  if (weighted)
    wt.var = "Weight"
  
  country.data <- country.data %>% mutate(
    Party = fct_drop(Party),
    "{group}" = fct_drop(.data[[group]])
  )
  
  if (length(unique(country.data[[group]])) <= 1) {
    return (NA)
  }
  
  assoc <- NULL
  try({
    assoc <- suppressWarnings(pw.assoc(as.formula(paste(group, "~ Party")), country.data, out.df = T, weights = wt.var))
  })
  
  assoc$tau
}

calc.gallagher <- function(party.sizes.by.grp, grp.sizes, party.sizes, loosmore = F, by.party = F) {
  # Calculate unweighted values for each party
  res <- party.sizes.by.grp %>% 
    inner_join(grp.sizes, by = "Group") %>%
    mutate(value = percent.x - percent.y)
  
  if (loosmore) {
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
  
  if (by.party)
    return (res %>% mutate(total = total*100))
  
  # Apply weights
  res.wt <- party.sizes %>%
    inner_join(res, by = "Party") %>%
    mutate(total = total * percent)
  
  res.wt %>% 
    ungroup() %>% 
    summarise(total = sum(total)) %>%
    pull(total) * 100
}

calc.gatev <- function(party.support.by.group, group.sizes, party.sizes, wt.by.party = F) {
  res <- party.support.by.group %>% 
    inner_join(group.sizes, by = "Group") %>%
    mutate(nom = (percent.y - percent.x)^2) %>%
    mutate(denom = (percent.y^2 + percent.x^2))
  
  # Unclear if this step is needed
  if (wt.by.party) {
    res <- party.sizes %>%
      inner_join(res, by = "Party") %>%
      group_by(Party) %>%
      mutate(gatev = sqrt(sum(nom) / sum(denom))) %>%
      distinct(Party, percent, gatev) %>%
      mutate(gatev = gatev * percent) %>%
      ungroup() %>%
      summarise(gatev = sum(gatev))
    
    return(res$gatev)
  }
      
  res %>% 
    ungroup() %>% 
    summarise(gatev = sqrt(sum(nom) / sum(denom))) %>%
    pull(gatev)
}

calc.huber.indices <- function(summary.data, group.sizes, party.sizes, group.sizes.by.pty, party.sizes.by.grp) {
  # Calculate differences in party support between each pair of groups
  rT <- expand_grid(unique(summary.data$Group), unique(summary.data$Group), unique(summary.data$Party), .name_repair = "minimal") %>%
    set_names("g1", "g2", "p") %>%
    left_join(group.sizes.by.pty, by = c('g1' = 'Group', 'p' = 'Party')) %>%
    left_join(group.sizes.by.pty, by = c('g2' = 'Group', 'p' = 'Party')) %>%
    mutate(across(c(percent.x, percent.y), ~if_else(is.na(.x), 0, .x))) %>%
    mutate(rT.init = percent.x - percent.y) %>%
    mutate(rT = rT.init^2)
  
  # Sum the differences by group dyad
  rT.sum <- rT %>% group_by(g1, g2) %>% summarise(rT.orig = sum(rT), .groups = "drop") %>%
    mutate(rT = sqrt(0.5*rT.orig))
  
  # Adjust differences by group size
  rT.sum <- rT.sum %>%
    inner_join(group.sizes, by = c('g1' = 'Group')) %>%
    rename('g1.group.sizes' = percent) %>%
    inner_join(group.sizes, by = c('g2' = 'Group')) %>%
    rename('g2.group.sizes' = percent) %>%
    mutate(VF = rT*g1.group.sizes*g2.group.sizes) %>%
    mutate(VP = rT*g1.group.sizes*g2.group.sizes^2)
  
  # Calculate differences in group support between each pair of parties
  rP <- expand_grid(unique(summary.data$Party), unique(summary.data$Party), unique(summary.data$Group), .name_repair = "minimal") %>%
    set_names("p1", "p2", "g") %>%
    left_join(party.sizes.by.grp, by = c('p1' = 'Party', 'g' = 'Group')) %>%
    left_join(party.sizes.by.grp, by = c('p2' = 'Party', 'g' = 'Group')) %>%
    mutate(across(c(percent.x, percent.y), ~if_else(is.na(.x), 0, .x))) %>%
    mutate(rP.init = percent.x - percent.y) %>%
    mutate(rP = rP.init^2)
  
  rP.sum <- rP %>% group_by(p1, p2) %>% summarise(rP.orig = sum(rP), .groups = "drop") %>%
    mutate(rP = sqrt(0.5*rP.orig))
  
  # Adjust differences by party size
  rP.sum <- rP.sum %>%
    inner_join(party.sizes, by = c('p1' = 'Party')) %>%
    rename('p1.party.sizes' = percent) %>%
    inner_join(party.sizes, by = c('p2' = 'Party')) %>%
    rename('p2.party.sizes' = percent) %>%
    mutate(PVF = rP*p1.party.sizes*p2.party.sizes) %>%
    mutate(PVP = rP*p1.party.sizes*p2.party.sizes^2)
  
  list(
    VF  = rT.sum %>% ungroup() %>% summarise(VF = sum(VF)) %>% pull(VF),
    VP  = rT.sum %>% ungroup() %>% summarise(VP = sum(VP)*4) %>% pull(VP),
    PVF = rP.sum %>% ungroup() %>% summarise(PVF = sum(PVF)) %>% pull(PVF),
    PVP = rP.sum %>% ungroup() %>% summarise(PVP = sum(PVP)*4) %>% pull(PVP)
  )
  
}