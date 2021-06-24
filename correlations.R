library(tidyverse)
library(GoodmanKruskal)
library(countrycode)

skip.countries <- c("CHN", "EGY", "VNM")

gen.correlations <- function(direction = "back") {
  data <- readRDS("Divided/data/orig/WVS_Cross-National_Wave_7_R_v1_6.rds") %>%
    filter(! B_COUNTRY_ALPHA %in% skip.countries)
  
  questions <- c("Q272", "Q289", "Q290")
  q.names <- c("Language", "Religion", "Ethnicity")
  names(q.names) <- questions
  
  res <- map_dfr(unique(data$B_COUNTRY_ALPHA), function(country) {
    d <- data %>%
      filter(B_COUNTRY_ALPHA == country)
    
    tau <- map_dfr(questions, function(var) {
      t <- GKtau(d[[var]], d$Q223)
      tibble(question = q.names[[var]], forward = t$tauxy, back = t$tauyx)
    }) 
    
    if (! is.na(direction)) {
      tau <- tau %>% select(question, {{direction}}) %>%
        pivot_longer({{direction}}) %>%
        pivot_wider(names_from = c(question)) %>%
        select(-name)
    }
    else {
      tau <- tau %>%
      pivot_longer(c(forward, back)) %>% 
      pivot_wider(names_from = c(question, name))
    }
    
    tau$country <- countrycode(country, "iso3c", "country.name")
    
    tau
    
  })
  
  maxes <- res %>% 
    pivot_longer(-country) %>%
    group_by(country) %>% 
    slice_max(value, n = 1, with_ties = F) %>%
    rename("Max Type" = name, "Max" = value)
  
  res <- inner_join(res, maxes, by = "country") %>%
    select(country, everything()) %>%
    mutate(across(c(-country, -`Max Type`), ~ifelse(
      is.nan(.x) | .x == -Inf, NA, .x
    ))) %>%
    arrange(desc(Max))
  
  return(res)
}