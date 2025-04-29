data.spec <- list(
  file.name = "ZA2900.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "D-W" = "Germany",
    "D-E" = "Germany",
    "nirl" = "United Kingdom",
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
    "IL-J" = "Israel",
    "IL-A" = "Israel",
    "j" = "Japan",
    "e" = "Spain",
    "lv" = "Latvia",
    "f" = "France",
    "cy" = "Cyprus",
    "ch" = "Switzerland"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v219",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v325"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(224:247))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class)", "Protestant (nec)", Religion)) %>%
      mutate(Year = 1996)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No party preference", "None", 
                  "Would not vote;No party preference", 
                  "No particular party", "Non Party Individuals",
                  "Would not vote;no party preference", "Would not vote,No party preference",
                  "Would not vote;not eligible", "No party preference;wouldnt vote",
                  "Would not vote,no party preference", "No preference,no vote",
                  "Would not vote"),
    "Other" = c("Other answer", "Other Party")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)