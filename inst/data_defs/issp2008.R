data.spec <- list(
  file.name = "ZA4950_v2-3-0.dta",
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
    "Belgium" = "PartyVote",
    "Czechia" = "PartyVote",
    "Germany" = "PartyVote",
    "Denmark" = "PartyVote",
    "Spain" = "PartyVote",
    "Finland" = "PartyVote",
    "United Kingdom" = "PartyVote",
    "Italy" = "PartyVote",
    "Netherlands" = "PartyVote",
    "Norway" = "PartyVote",
    "New Zealand" = "PartyVote",
    "Poland" = "PartyVote",
    "Russia" = "PartyVote",
    "Slovenia" = "PartyVote",
    "Slovakia" = "PartyVote",
    "South Africa" = "PartyVote"
  ),
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom",
    "BE-FLA" = "Belgium",
    "DE-E" = "Germany",
    "DE-W" = "Germany",
    "IL (A)" = "Israel",
    "IL (J)" = "Israel"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "RELIG",
    "Language" = NA,
    "Ethnicity" = "ETHNIC",
    "Country" = "C_ALPHAN",
    "Year" = "Year",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY")), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(Year = 2008) %>%
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty))
    
    data
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "No party preference", "No party affiliation", 
                   "Did not vote", "No party, no preference", "Don't remember",
                  "Vote for a certain person not for a specific party",
                  "No answer", "Don't know", "None", "No answer, refused",
                  "Did not vote, not eligible", "Would not vote",
                  "None, no party preference", "Will not vote",
                  "None, does not identify with any party", "Uncertain",
                  "Would not vote; no party preference", "I would not go to vote",
                  "None of these; no party preference", "Don't know, don't remember",
                  "Not eligible to vote, as not a German citizen",
                  "No one; no party preference", "Would not vote, no preference",
                  "Vote blank", "Vote blank, not valid", "Would not vote, no party at all",
                  "Voted blank", "No answer, don't know", "Against all/ threw out/ damaged voting paper",
                  "DK; Not interested so much in politics", "Didnt vote/not eligible (code 2 in vote_le)"),
    "Other" = c("Other party", "Independent", "Other answer", "Other Party",
                "Other Party: Healthcare, Feministic Init, June List",
                "Other Parties", "Other (specify)", "Other party, specify",
                "Other Party, Socialist Party", "Other Regional Party")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know"),
    "Other" = c("Other Religions", "Other non-Christian Religions"),
    "No Religion" = c("No religion, HU: including not religious")
  ),
  Ethnicity = list(
    "Missing" = c("NAP; NAV", "No answer, don't know"),
    "Other" = c("34.1", "82.2", "82.1", "34.2", "Other,mixed origin")
  )
)