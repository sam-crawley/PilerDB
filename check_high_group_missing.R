
get.high.group.missing <- function(tabs) {
  map_dfr(names(tabs$crosstabs), function (country) {
    sum.line = tabs$summary %>% filter(ID == country)
    
    if (is.na(sum.line$`Group Basis`))
      return (NULL)
    
    map_dfr(group.names, function (group) {
      summary.data <- tabs$crosstabs[[country]][[group]]
      
      summary.data <- config.summary.data(summary.data, drop.cats = F, weighted = T)
      
      if (! is.data.frame(summary.data))
        return (NULL)
      
      high.missing <- summary.data %>% 
        filter(! Group %in% cats.to.drop) %>%
        group_by(Group) %>% 
        mutate(missing.percent = n / sum(n), total.percent = sum(n) / tabs$crosstabs[[country]]$Summary$general$`Sample Size`) %>% 
        filter(Party == "Missing" & total.percent >= 0.02 & missing.percent >= 0.9)
      
      if (nrow(high.missing) > 0) {
        return (tibble(
          datasource = country,
          group = group,
          group.name = high.missing$Group
        ))
      }
      
      return (NULL)
        
    })
  })
}
