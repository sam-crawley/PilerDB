# This adds "warning flags" to individual surveys if we suspect
#  there are problems with the data

add.warning.flags <- function(crosstabs) {
  # Add "high group missing" flag
  hgm <- get.high.group.missing(crosstabs) %>%
    select(datasource, flag) %>% 
    distinct()
  
  map(names(crosstabs), function (country) {
    crosstabs[[country]]$Summary$general$warning.flags <- NA
    
    hgm.country <- hgm %>% filter(datasource == country)
    
    if (nrow(hgm.country) == 1 && hgm.country$flag)
      crosstabs[[country]]$Summary$general$warning.flags <- "high_group_missing"
    
    crosstabs[[country]]
  }) %>% set_names(names(crosstabs))
  
}


# Find cases where a group has a particularly high number of missing
#  responses for the party variable.
# We determine this by:
# 1. Look only at the 'Group Basis' (i.e. highest tau) group
# 2. Calculate the % size of the group, and % of missing on party
# 3. For each group, compare their missing % to
#     the missing % of the largest group, or if it is the
#     largest, the 2nd largest group. If missing % difference is
#     is larger than 20%, the survey is "flagged"
# 4. Remove groups smaller than 10% or less than 5% missing
get.high.group.missing <- function(crosstabs, flagged.only = T) {
  res <- map_dfr(names(crosstabs), function (country) {
    summary = crosstabs[[country]]$Summary$general
    
    if (is.na(summary$`Group Basis`))
      return (NULL)
    
    group <- summary$`Group Basis`
    
    summary.data <- crosstabs[[country]][[group]]
    
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
    
    if (flagged.only)
      high.missing <- high.missing %>% filter(flag == T)
    
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
