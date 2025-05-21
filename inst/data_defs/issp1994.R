data.spec <- list(
  file.name = "ZA2620.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Germany" = "PartyVote",
    "Spain" = "PartyVote",
    "Hungary" = "PartyVote",
    "Italy" = "PartyVote",
    "Ireland" = "PartyVote",
    "Norway" = "PartyVote",
    "Poland" = "PartyVote",
    "Slovenia" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "NIRL- Northern Ireland" = "United Kingdom",
    "D-E - Germany-East" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v219",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v315"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(224:245))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class)", "Protestant (nec)", Religion)) %>%
      mutate(Year = 1994)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No preference,no vote", "Would not vote;No party preference",
                  "No party preference", "No party no preference", "Not eligible",
                  "No particular party", "No specific preference", "Would not vote"),
    "Other" = c("Other answer", "Other Party", "Other Parties", "Party on left",
                "Party on right", "Other Union")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)