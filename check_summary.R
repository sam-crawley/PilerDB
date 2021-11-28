check.cases <- function(summary) {
  best.groups <- summary %>% 
    group_by(Country) %>% 
    filter(cor.nomiss == max(cor.nomiss, na.rm = T)) %>% 
    select(Country, `Group Basis`, cor.nomiss) %>%
    rename(
      "Best Group" = `Group Basis`,
      "Highest Corr" = cor.nomiss
    )
  
  problems <- summary %>%
    inner_join(best.groups, by = "Country") %>%
    mutate("Best Group Available" = case_when(
      `Best Group` == "Language" ~ Language,
      `Best Group` == "Religion" ~ Religion,
      `Best Group` == "Ethnicity" ~ Ethnicity,
    )) %>%
    filter(`Best Group` != `Group Basis`) %>%
    select(ID, Country, Year, `Group Basis`, `Best Group`, `Best Group Available`, cor.nomiss, `Highest Corr`) %>%
    mutate(
      "Correlation Difference" = abs(`Highest Corr` - cor.nomiss)
    ) %>%
    rename("Correlation" = cor.nomiss) %>%
    filter(`Correlation Difference` >= 0.1)
  
  problems
    
}

check.cases.by.hampel <- function(summary) {
  ranges <- summary %>% 
    group_by(Country) %>% 
    summarise(median = median(cor.nomiss, na.rm = T), mad = mad(cor.nomiss, constant = 1, na.rm = T)) %>% 
    mutate(min = median - 3 * mad, max = median + 3 * mad)
  
  problems <- inner_join(sum, ranges, by = "Country") %>% 
    mutate(outside.range = cor.nomiss < min | cor.nomiss > max) %>%
    filter(outside.range)
  
  problems
}