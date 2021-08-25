#ethnicvoting %>%
#  filter(grouptype == "fearon") %>%
#  ggplot(aes(y = VF, x = PVF, label = ctry_yr)) +
#  geom_point() + 
#  geom_text(hjust = -0.1) +
#  xlim(0, 0.5) +
#  ylim(0, 0.5) +
#  geom_abline(intercept = 0, slope = 1)


# Benin, VF=0.184917509555817

benin.data <- load.data.by.id("AFB3", process = F) %>%
  filter(Country == "Benin") %>%
  filter(Ethnicity %in% c("Fon", "Bariba", "Yoruba", "Peulh")) %>%
  mutate(Party2 = haven::as_factor(q99)) %>%
  filter(Party2 %in% c('Nicéphore SOGLO', 'Adrien HOUNGBEDJI', 'KEREKOU', 'Yayi Boni', 'Séfou FAGBOHOUN', 'Bruno AMOUSSOU'))
  
apct <- benin.data %>%
  group_by(Ethnicity) %>%
  filter(! is.na(Party)) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / sum(n)) %>%
  select(Ethnicity, pct)

pct <- benin.data %>%
  group_by(Ethnicity, Party2) %>%
  count() %>%
  group_by(Ethnicity) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n)

rT <- expand_grid(unique(benin.data$Ethnicity), unique(benin.data$Ethnicity), unique(benin.data$Party2), .name_repair = "minimal") %>%
  set_names("g1", "g2", "p") %>%
  left_join(pct, by = c('g1' = 'Ethnicity', 'p' = 'Party2')) %>%
  left_join(pct, by = c('g2' = 'Ethnicity', 'p' = 'Party2')) %>%
  mutate(across(c(pct.x, pct.y), ~if_else(is.na(.x), 0, .x))) %>%
  mutate(rT.init = pct.x - pct.y) %>%
  mutate(rT = rT.init^2)

rT.sum <- rT %>% group_by(g1, g2) %>% summarise(rT.orig = sum(rT)) %>%
  mutate(rT = sqrt(0.5*rT.orig))

rT.sum <- rT.sum %>%
  inner_join(apct, by = c('g1' = 'Ethnicity')) %>%
  rename('g1.apct' = pct) %>%
  inner_join(apct, by = c('g2' = 'Ethnicity')) %>%
  rename('g2.apct' = pct) %>%
  mutate(VF = rT*g1.apct*g2.apct)

rT.sum %>% ungroup() %>% summarise(VF = sum(VF))
