data.spec <- list(
  file.name = "ZA3440.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Czechia" = "PartyVote",
    "Germany" = "PartyVote",
    "Denmark" = "PartyVote",
    "Spain" = "PartyVote",
    "Ireland" = "PartyVote",
    "Mexico" = "PartyVote",
    "Netherlands" = "PartyVote",
    "Russia" = "PartyVote",
    "Slovenia" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "NIRL-Northern Ireland" = "United Kingdom",
    "D-E-Germany-East" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v242",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = "Year",
    "Weight" = "v327"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(247:271))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data <- data %>%
      mutate(Year = 2000)
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class), reformated", "Protestant (nec)", Religion)) %>%
      filter(Weight != 0)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote", "None", 
                   "Would not vote;no party preference", "Would not vote;not eligible",
                   "Would not vote;No party preference", "No party preference;wouldnt vote", 
                   "No preference, not vote", "No vote;no party preference",
                   "Would not vote,no party preference", "No preference",
                   "None, no preference", "No preference,no vote", "Vote blank",
                   "Vote blank,not eligible"),
    "Other" = c("Other party", "Other answer", "Other Party", "Independent Candidates")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)