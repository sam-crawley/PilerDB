library(haven)
library(countrycode)

wvs7.skip.countries <- c("CHN", "EGY", "VNM", "JOR")

wvs7.cats <- list(
  Party = list(
    "Missing" = c("Not applicable", "No answer", "Don\ub4t know", "No right to vote", "I would not vote", "Missing",
                                "I would cast a blank ballot; White vote", "None", "Null vote"),
    "Other" = c("Other", "Independent candidate")
  ),
  Language = list(
    "Other" = c("Other", "Other European", "Other Chinese dialects", "Other local; aboriginal; tribal, community")
  ),
  Religion = list(
    "Other" = c("Other", "Other Christian (Pentecostal/Free church/Jehova...)", "Do not belong to a denomination")
  ),
  Ethnicity = list(
    "Other" = c("Other, non-Hispanic", "Two plus, non-Hispanic")
  )
)

read.data.wvs <- function() {
  data <- read_dta("Divided/data/orig/WVS_Cross-National_Wave_7_stata_v1_6_2.dta", encoding = "UTF-8") %>%
    filter(! B_COUNTRY_ALPHA %in% wvs7.skip.countries)
  
  data <- data %>%
    mutate(across(c(Q223, Q272, Q289, Q290), haven::as_factor)) %>%
    mutate(across(c(Q223, Q272, Q289, Q290), ~fct_explicit_na(.x, na_level = "Missing"))) %>%
    rename(
      "Party" = Q223,
      "Language" = Q272,
      "Religion" = Q289,
      "Ethnicity" = Q290,
      "Country.Code" = B_COUNTRY_ALPHA,
      "Year" = A_YEAR,
      "Weight" = W_WEIGHT
    ) %>%
    mutate("Country" = countrycode(Country.Code, origin = 'iso3c', destination = 'country.name'))  %>%
    arrange(Country)
  
  # Strip out country prefixes from levels
  for (var in main.vars) {
    levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\w+:\\s*")
  }

  return(data)
}