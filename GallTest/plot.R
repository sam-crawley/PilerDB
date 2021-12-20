gen.simple.index.plot <- function() {
  steps <- 0:10*5
  
  data <- map_dfr(steps, function(s) {
    d <- gen.party.by.grp.df(s)
    calc.summary.indices(d) %>%
      mutate(step = s) %>%
      mutate(across(c(gallagher, loosemore), ~.x/100)) %>%
      select(step, gallagher, loosemore, PVP)
  })
  
  data %>% pivot_longer(-step) %>%
    ggplot(aes(x = step, y = value, color = name)) +
    geom_line(size = 1.1, position = position_dodge(width = 1.5)) + 
    theme_minimal() + 
    ylim(0, 1)
}

gen.group.num.plot <- function() {
  group.count <- 2:10
  
  data <- map_dfr(group.count, function(g.count) {
    data <- expand_grid(Party = 1:g.count, Group = 1:g.count) %>% 
      mutate(n = if_else(Party == Group, 100, 0))
    
    calc.summary.indices(data) %>%
      mutate(group.count = g.count) %>%
      mutate(across(c(gallagher, loosemore), ~.x/100)) %>%
      select(group.count, gallagher, loosemore, PVP)
  })
  
  data %>% pivot_longer(-group.count) %>%
    ggplot(aes(x = group.count, y = value, color = name)) +
    geom_line(size = 1.1) + 
    theme_minimal() + 
    ylim(0, 1)
}

gen.party.num.plot <- function() {
  party.count <- c(1:8)*2
  
  data <- map_dfr(party.count, function(p.count) {
    data <- expand.grid(Party = 1:p.count, Group = 1:2) %>%
      mutate(n = if_else(Party %% 2 == Group-1, 100, 0))
    
    calc.summary.indices(data) %>%
      mutate(party.count = p.count) %>%
      mutate(across(c(gallagher, loosemore), ~.x/100)) %>%
      select(party.count, gallagher, loosemore, PVP)
  })
  
  data %>% pivot_longer(-party.count) %>%
    ggplot(aes(x = party.count, y = value, color = name)) +
    geom_line(size = 1.1, position = position_dodge(width = 1.5)) + 
    theme_minimal() + 
    ylim(0, 1)  
}