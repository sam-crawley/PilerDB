tmp <- map_dfr(names(tabs$crosstabs), function (country) {
  gb <- tabs$crosstabs[[country]]$Summary$general$`Group Basis`
  
  cor.nomiss <- tabs$crosstabs[[country]]$Summary$cor.nomiss.wt %>% filter(group == gb)
  
  gall.wt <- NA
  if (nrow(cor.nomiss) > 0)
    gall.wt <- cor.nomiss %>% pull(gallagher)
  
  tibble(
    ID = country,
    gall.wt = gall.wt
  )
})

tmp <- inner_join(tabs$summary, tmp, by = "ID") %>%
  mutate(gall.diff = abs(Gallagher - gall.wt)) %>%
  select(ID, Gallagher, gall.wt, gall.diff)
