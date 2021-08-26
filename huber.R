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
  
# Calculate percentage size of each group (who have cast a vote)
apct <- benin.data %>%
  group_by(Ethnicity) %>%
  filter(! is.na(Party)) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / sum(n)) %>%
  select(Ethnicity, pct)

# Calculate percentage size of each party
party.pct <- benin.data %>%
  group_by(Party2) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / sum(n)) %>%
  select(Party2, pct)

# Calculate percentage of each group that voted for each party
pct <- benin.data %>%
  group_by(Ethnicity, Party2) %>%
  count() %>%
  group_by(Ethnicity) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n)

# Calculate percentage of votes received by each party
pct.by.party <- benin.data %>%
  group_by(Ethnicity, Party2) %>%
  count() %>%
  group_by(Party2) %>%
  mutate(pct = n / sum(n)) %>%
  select(-n)

# Calculate differences in party support between each pair of groups
rT <- expand_grid(unique(benin.data$Ethnicity), unique(benin.data$Ethnicity), unique(benin.data$Party2), .name_repair = "minimal") %>%
  set_names("g1", "g2", "p") %>%
  left_join(pct, by = c('g1' = 'Ethnicity', 'p' = 'Party2')) %>%
  left_join(pct, by = c('g2' = 'Ethnicity', 'p' = 'Party2')) %>%
  mutate(across(c(pct.x, pct.y), ~if_else(is.na(.x), 0, .x))) %>%
  mutate(rT.init = pct.x - pct.y) %>%
  mutate(rT = rT.init^2)

# Sum the differences by group dyad
rT.sum <- rT %>% group_by(g1, g2) %>% summarise(rT.orig = sum(rT)) %>%
  mutate(rT = sqrt(0.5*rT.orig))

# Adjust differences by group size
rT.sum <- rT.sum %>%
  inner_join(apct, by = c('g1' = 'Ethnicity')) %>%
  rename('g1.apct' = pct) %>%
  inner_join(apct, by = c('g2' = 'Ethnicity')) %>%
  rename('g2.apct' = pct) %>%
  mutate(VF = rT*g1.apct*g2.apct)

# Calculate VF
rT.sum %>% ungroup() %>% summarise(VF = sum(VF))

# Calculate VP
rT.sum <- rT.sum %>%
  mutate(VP = rT*g1.apct*g2.apct^2)

rT.sum %>% ungroup() %>% summarise(VP = sum(VP)*4)

# Calculate differences in group support between each pair of parties
rP <- expand_grid(unique(benin.data$Party2), unique(benin.data$Party2), unique(benin.data$Ethnicity), .name_repair = "minimal") %>%
  set_names("p1", "p2", "g") %>%
  left_join(pct.by.party, by = c('p1' = 'Party2', 'g' = 'Ethnicity')) %>%
  left_join(pct.by.party, by = c('p2' = 'Party2', 'g' = 'Ethnicity')) %>%
  mutate(across(c(pct.x, pct.y), ~if_else(is.na(.x), 0, .x))) %>%
  mutate(rP.init = pct.x - pct.y) %>%
  mutate(rP = rP.init^2)

rP.sum <- rP %>% group_by(p1, p2) %>% summarise(rP.orig = sum(rP)) %>%
  mutate(rP = sqrt(0.5*rP.orig))

# Adjust differences by party size
rP.sum <- rP.sum %>%
  inner_join(party.pct, by = c('p1' = 'Party2')) %>%
  rename('p1.pct' = pct) %>%
  inner_join(party.pct, by = c('p2' = 'Party2')) %>%
  rename('p2.pct' = pct) %>%
  mutate(PVF = rP*p1.pct*p2.pct)

# Calculate PVF
rP.sum %>% ungroup() %>% summarise(PVF = sum(PVF))

# Calculate PVP
rP.sum <- rP.sum %>%
  mutate(PVP = rP*p1.pct*p2.pct^2)

rP.sum %>% ungroup() %>% summarise(PVP = sum(PVP)*4)