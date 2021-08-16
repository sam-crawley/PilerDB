data.spec <- list(
  file.name = "Divided/data/asain barom/W1 Merged Data/Wave.1_Data/Merge/Wave1_20170906.sav",
  file.type = 'sav',
  skip.countries = c('China', 'Hong Kong SAR China', 'Philippines'),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q028",
    "Religion" = "se006",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "country",
    "Year" = NA,
    "Weight" = "w_all"
  ),
  fixups = function(data) {
    data %>% mutate(Year = case_when(
      Country == "Taiwan" ~ 2001,
      Country == "Philippines" ~ 2002,
      Country == "Mongolia" ~ 2002,
      Country == "Thailand" ~ 2001,
      Country == "Japan" ~ 2003,
      Country == "South Korea" ~ 2003,
      TRUE ~ 0
    ))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "No answer", "Forgot", "Not affiliated"),
    "Other" = c("other", "Other parties")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("None", "Others")
  )
)