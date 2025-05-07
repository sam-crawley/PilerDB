data.spec <- list(
  file.name = "ZA3190.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  country.custom = c(
    "NIRL-Northern Ireland" = "United Kingdom",
    "D-E-Germany-East" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v217",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v316"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(222:251))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class)", "Protestant (nec)", Religion)) %>%
      mutate(Year = 1998)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote", "None", 
                   "No preference", "Would not vote,No party preference",
                   "Would not vote;not eligible", "Would not vote,no party preference",
                   "Would not vote;no party preference", "Would not vote;No party preference",
                   "No vote,not eligible", "None, no preference",
                   "Not vote", "No preference,no vote", "Vote blank",
                   "No party preference;wouldnt vote", "No vote;no party preference",
                   "No preference, not vote"),
    "Other" = c("Other party", "Other answer", "Other Party",
                "Other left party", "Other right party", "Other religious party",
                "Independents", "Vote for an independent cand", "Other responses")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)