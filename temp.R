data <- load.RDS("~/RA/Divided/WVS_Cross-National_Wave_7_R_v1_6.rds")
tmp <- lbn %>%
  mutate(Q223 = as_factor(Q223)) %>%
  count(Q223)
