data.spec <- list(
  file.name = "ZA3880_v1-1-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Austria" = "PartyVote",
    "Czechia" = "PartyVote",
    "Germany" = "PartyVote",
    "Denmark" = "PartyVote",
    "Spain" = "PartyVote",
    "Ireland" = "PartyVote",
    "Mexico" = "PartyVote",
    "Netherlands" = "PartyVote",
    "Poland" = "PartyVote",
    "Russia" = "PartyVote",
    "Slovenia" = "PartyVote"
  ),
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom",
    "BE-FLA" = "Belgium",
    "DE-E" = "Germany",
    "DE-W" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "v288",
    "Language" = NA,
    "Ethnicity" = "v359",
    "Country" = "C_ALPHAN",
    "Year" = "Year",
    "Weight" = "v361"
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- paste0("v", c(254:286))
    data <- data %>%
      mutate(across(all_of(party.vars), ~if_else(.x == 0, NA, .x))) %>%
      coalese.vars(party.vars, "prty")
    
    data <- data %>%
      mutate(Year = 2002) %>%
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote",
                   "None",  "Would not vote;no party preference",
                   "None, no preference", "Would not vote;not eligible",
                   "No preference",  "Would not vote;No party preference",
                   "No preference,no vote", "No party preference, none",
                   "Would not vote,no party preference", "Don't  support any party",
                   "Don't  support any party", "No party preference, not vote",
                   "Did not vote", "No party preference;wouldnt vote", "Vote blank",
                   "No vote,not eligible", "No preference, not vote"),
    "Other" = c("Other party", "Other answer", "Other Party", "Independyente")
  ),
  Religion = list(
    "Missing" = c("No answer"),
    "Other" = c("Other Religions", "Other non-christian Religions"),
    "No Religion" = c("None, no Religion")
  ),
  Ethnicity = list(
    "Missing" = c("NAP,no nationality;NAV", "Na, Don't  know"),
    "Other" = c("Other,mixed origin,one-non-swedish", "Both non-Swedish")
  )
)