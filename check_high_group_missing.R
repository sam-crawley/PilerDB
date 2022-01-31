
get.high.group.missing <- function(tabs) {
  res <- map_dfr(names(tabs$crosstabs), function (country) {
    sum.line = tabs$summary %>% filter(ID == country)
    
    if (is.na(sum.line$`Group Basis`))
      return (NULL)
    
    group <- sum.line$`Group Basis`
    
    summary.data <- tabs$crosstabs[[country]][[group]]
    
    summary.data <- config.summary.data(summary.data, drop.cats = F, weighted = T)
    
    if (! is.data.frame(summary.data))
      return (NULL)
    
    sample.size <- sum(summary.data$n)
    
    high.missing <- summary.data %>% 
      filter(! Group %in% cats.to.drop) %>%
      group_by(Group) %>% 
      mutate(missing.percent = n / sum(n), total.percent = sum(n) / sample.size) %>% 
      filter(Party == "Missing") %>%
      ungroup()
    
    if (nrow(high.missing) <= 1)
      return (NULL)
    
    mp <- high.missing %>% arrange(desc(total.percent))
    top.group <- mp$Group[[1]]
    
    high.missing <- high.missing %>%
            mutate(comp.group = if_else(Group == top.group, mp$Group[[2]], top.group)) %>%
            mutate(max.percent = if_else(Group == top.group, mp$missing.percent[[2]], mp$missing.percent[[1]])) %>%
            mutate(missing.diff = missing.percent - max.percent) %>%
            mutate(flag = if_else(missing.diff >= 0.20, T, F)) %>%
            filter(total.percent >= 0.10 & missing.percent >= 0.5)
    
    if (nrow(high.missing) > 0) {
      return(
        high.missing %>% 
          mutate(datasource = country, group = group) %>%
          rename(
            missing = missing.percent,
            group.name = Group,
            group.size = total.percent,
            comp.missing = max.percent
          ) %>%
          select(
            datasource,
            group,
            group.name,
            group.size,
            missing,
            comp.group,
            comp.missing,
            missing.diff,
            flag
          )
      )
    }
    
    return (NULL)
  })
  
  res %>% mutate(across(c(group.size, missing, comp.missing, missing.diff), ~round(.x, 3))) %>%
    arrange(desc(missing))
}
