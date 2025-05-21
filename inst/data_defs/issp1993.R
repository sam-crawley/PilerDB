data.spec <- list(
  file.name = "ZA2450.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Germany" = "PartyVote",
    "Hungary" = "PartyVote",
    "Ireland" = "PartyVote",
    "Norway" = "PartyVote",
    "Slovenia" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "Northern Ireland - NIRL" = "United Kingdom",
    "Germany-East - D-E" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v276",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v419"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(282:303))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data
  },
  fixups = function(data) {
    data %>% 
      mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class)", "Protestant (nec)", Religion)) %>%
      mutate(Year = 1993)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Would not vote;No party preference", "No party preference",
                  "Would n vote;No party preference", "Would not vote;No preference"),
    "Other" = c("Other answer", "Other Party", "Party on left", "Party on right",
                "Left national parties", "Other Union.")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)