data.spec <- list(
  file.name = "ZA3950_v1-3-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "BE-FLA" = "Belgium",
    "DE-E" = "Germany",
    "DE-W" = "Germany",
    "IL (J+A)" = "Israel"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v298",
    "Language" = NA,
    "Ethnicity" = "v379",
    "Country" = "C_ALPHAN",
    "Year" = "Year",
    "Weight" = "v381"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(259:296))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(Year = 2004) %>%
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty))
    
    data
  },
  fixups = function(data) {
    #data %>%
    #  mutate(across(c(Party, Religion, Ethnicity), ~str_replace(.x, "^\\d+\\.\\s+", "")))
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote", "Don't support any party",
                  "None", "Would not vote,no preference", "Would not vote;no party preference",
                  "No party preference, none", "Wouldn't vote,no party preference",
                  "None, no preference", "No party preference,no vote",
                  "Would not vote, no party", "Vote blank", "No party preference;wouldnt vote",
                  "Against all,damaged voting paper"),
    "Other" = c("Other party", "Other answer", "Other Party", "Independents",
                "Other list")
  ),
  Religion = list(
    "Missing" = c("Don't know", "NA, refused"),
    "Other" = c("Other Religions"),
    "No Religion" = c("No religion")
  ),
  Ethnicity = list(
    "Missing" = c("NAP; NAV", "NA, don't know"),
    "Other" = c("Mixed origin,ZA:Coloured, other")
  )
)