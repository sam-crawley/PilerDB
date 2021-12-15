gall.country.scores <- function(tabs) {
  res <- map_dfr(tabs$crosstabs, function(country.data) {
    if (is.na(country.data$Summary$general$`Group Basis`)) {
      return(NULL)
    }
    
    summary.data <- config.summary.data(country.data[[country.data$Summary$general$`Group Basis`]], drop.cats = T)
    
    index.summaries <- build.index.summary.data(summary.data)
    
    index.summaries$party.support.by.group %>% 
      inner_join(index.summaries$group.sizes, by = "Group") %>%
      mutate(value = percent.x - percent.y) %>%
      mutate(value = value^2) %>% 
      group_by(Party) %>% 
      summarise(total = round(sqrt(sum(value)/2), 3)) %>%
      rename(Gallagher = total) %>%
      mutate(p.no = c(1:n())) %>% 
      mutate(ID = country.data$Summary$general$ID)

      
  })
  
  p.count <- max(res$p.no)
  
  p.cols <- paste0("Party_", c(1:p.count))
  g.cols <- paste0("Gallagher_", c(1:p.count))
  
  res %>% 
    pivot_wider(names_from = p.no, values_from = c(Party,Gallagher)) %>% 
    select(ID, all_of(c(rbind(p.cols,g.cols))))
  
}