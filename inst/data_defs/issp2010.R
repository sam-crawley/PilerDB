data.spec <- list(
  file.name = "ZA5500_v3-0-0.dta",
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
   "BE-FLA" = "Belgium"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = "ethn",
    "Country" = "c_alphan",
    "Year" = "DATEYR",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(ends_with("RELIG"), ~if_else(.x == 990, NA, .x))) %>%
      mutate(across(c(ends_with("PRTY"), ends_with("ETHN")), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    data <- coalese.vars(data, str_subset(names(data), "RELIG$"), "relig")
    data <- coalese.vars(data, str_subset(names(data), "ETHN$"), "ethn")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(prty = if_else(c_alphan == "US", "Missing", prty)) %>%
      mutate(DATEYR = if_else(DATEYR == 9999, NA, DATEYR))
    
    data
  },
  fixups = function(data) {
    # Remove unusable ethnicity questions
    data %>% mutate(Ethnicity = 
                      if_else(Country %in% c("Iceland", "Sweden", "Australia", "Norway", "Argentina"), "Missing", Ethnicity))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "No answer", "Vote blank, not valid",
                  "Don't know", "None", "Other party, specify", "Unspecified",
                  "Don't know, no answer, refused", "No party preference",
                  "No party affiliation", "Would not vote", "Not eligible to vote",
                  "Not interested so much in politics", "Not eligible", "Did not vote",
                  "No answer, did not vote", "Did not vote/ not eligible (code 2 in VOTE_LE)",
                  "No party affiliation; none, would not vote", "No party affiliation; would not vote",
                  "Uncertain, don't know", "No party affiliation, did not vote"),
    "Other" = c("Other party", "Other Party", "Independent", "Other answer", 
                "Vote for a certain person, not for a specific party", "Other parties",
                "Independent Candidate")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know", "Can't say"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other religious denomination", "Other Non-Christian Religions",
                "Other Religions", "Other non-Christian", "Other Non-Christians",
                "Other non-Christian Religions", "Other Non-Christian", "None Sectarian",
                "Other Religion"),
    "No Religion" = c("No religion", "None", "No denomination or life stance organisation")
  ),
  Ethnicity = list(
    "Missing" = c("Refused", "No answer", "No answer, refused", "Don't know",
                  "None of these ethnic groups", "Not belonging to an indigenous group"),
    "Other" = c("Other, mixed origin", "[Other] Developing world",
                "Two nationalities, non Swiss",
                "Other nationa/ ethnic identity", "Two citizenships", "Uncodeable and NAP")
  )
)