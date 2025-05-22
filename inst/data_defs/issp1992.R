data.spec <- list(
  file.name = "ZA2310.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "D-W" = "Germany",
    "D-E" = "Germany",
    "gb" = "United Kingdom",
    "usa" = "United States",
    "a" = "Austria",
    "h" = "Hungary",
    "i" = "Italy",
    "n" = "Norway",
    "s" = "Sweden",
    "cz" = "Czechia",
    "slo" = "Slovenia",
    "pl" = "Poland",
    "bg" = "Bulgaria",
    "rus" = "Russia",
    "nz" = "New Zealand",
    "cdn" = "Canada",
    "rp" = "Philippines"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v123",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v176"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    coalese.vars(data, str_subset(names(data), "122$"), "prty")
  },
  fixups = function(data) {
    data %>% 
      mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Year = 1992) %>%
      filter(Weight != 0)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No party preference", "would not vote", "None",
                  "No party, would not vote", "Would not vote"),
    "Other" = c("Other answer", "Other party", "Other parties")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denomination given", "Other not classified"),
    "No Religion" = c("None")
  )
)