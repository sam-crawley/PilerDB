library(tidyverse)
library(GoodmanKruskal)

skip.countries <- c("CHN", "EGY", "VNM")

gen.correlations <- function() {
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
    }) %>%
      pivot_longer(c(forward, back)) %>% 
      pivot_wider(names_from = c(question, name))
    
    tau$country <- country
    
    tau
    
  })
  
  maxes <- res %>% 
    pivot_longer(-country) %>% 
    group_by(country) %>% 
    filter(value == max(value)) %>%
    rename("Max Type" = name, "Max" = value)
  
  res <- inner_join(res, maxes, by = "country") %>%
    select(country, everything())
  
  return(res)
}