
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
      filter(Party == "Missing")
    
    mp <- high.missing %>% arrange(desc(total.percent)) %>% pull(missing.percent) %>% set_names(high.missing$Group)
    
    if (length(mp) <= 1)
      return (NULL)
    
    high.missing <- high.missing %>%
             mutate(max.percent = if_else(names(mp)[[1]] == Group, mp[[2]], mp[[1]])) %>%
             filter(missing.percent - max.percent > -0.2 & total.percent >= 0.10 & missing.percent >= 0.5)
    
    if (nrow(high.missing) > 0) {
      return (tibble(
        datasource = country,
        group = group,
        group.name = high.missing$Group,
        group.size = high.missing$total.percent,
        missing = high.missing$missing.percent
      ))
    }
    
    return (NULL)
  })
  
  res %>% mutate(across(c(group.size, missing), ~round(.x, 3))) %>%
    arrange(desc(missing))
}
