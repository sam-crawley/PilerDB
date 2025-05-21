data.spec <- list(
  file.name = "ZA2880.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Austria" = "PartyVote",
    "Czechia" = "PartyVote",
    "Germany" = "PartyVote",
    "Spain" = "PartyVote",
    "Hungary" = "PartyVote",
    "Italy" = "PartyVote",
    "Ireland" = "PartyVote",
    "Norway" = "PartyVote",
    "Netherlands" = "PartyVote",
    "New Zealand" = "PartyVote",
    "Russia" = "PartyVote",
    "Slovenia" = "PartyVote"
  ),
  
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
    "irl" = "Ireland",
    "nl" = "Netherlands",
    "n" = "Norway",
    "s" = "Sweden",
    "cz" = "Czechia",
    "slo" = "Slovenia",
    "pl" = "Poland",
    "bg" = "Bulgaria",
    "rus" = "Russia",
    "nz" = "New Zealand",
    "cdn" = "Canada",
    "rp" = "Philippines",
    "j" = "Japan",
    "e" = "Spain",
    "lv" = "Latvia",
    "sk" = "Slovakia"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v265",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v342"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(270:292))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class)", "Protestant (nec)", Religion)) %>%
      mutate(Year = 1995)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Would not vote;no party preference", "No preference,no vote",
                  "Not eligible, blanc vote", "Would not vote,no party preference"),
    "Other" = c("Other answer", "Other Party", "comb.center-right", "comb.center parties",
                "comb.left-center", "comb.left parties", "comb.right parties")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)