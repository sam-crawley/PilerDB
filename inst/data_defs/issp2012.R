data.spec <- list(
  file.name = "ZA5900_v4-0-0.dta",
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
   "GB-GBN" = "United Kingdom"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = "ethn",
    "Country" = "C_ALPHAN",
    "Year" = "DATEYR",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY"), ends_with("RELIG")), ~if_else(.x == 990, NA, .x))) %>%
      mutate(across(ends_with("ETHN"), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    data <- coalese.vars(data, str_subset(names(data), "RELIG$"), "relig")
    data <- coalese.vars(data, str_subset(names(data), "ETHN$"), "ethn")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(DATEYR = if_else(DATEYR == 9999, NA, DATEYR))
    
    data
  },
  fixups = function(data) {
    # Remove unusable ethnicity questions
    data %>% mutate(Ethnicity = 
                      if_else(Country %in% c("Iceland", "Mexico", "Australia", "France", "Norway"), "Missing", Ethnicity))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "Refused", "No answer", "Don't know, don't remember",
                  "Don't remember", "No party affiliation", "Don't know", "Can't say",
                  "Blank or invalid ballot", "No answer, refused", "Invalid ballot, can't say",
                  "Blank vote", "Invalid ballot, vote blank", "None"),
    "Other" = c("Other party", "Other Party", "Mixed vote (Candidates of several lists)",
                "Other - Multiple parties selected", "Independent", "Other answer",
                "Other parties")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other religious denomination", "Other religion", "Other religion",
                "Other Religions", "Other non-Christian", "Other non-Christian denomination",
                "Other not christian", "Other non-Christian religions",
                "Other non-Christian religion"),
    "No Religion" = c("No denomination or life stance organisation", "None")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "Undecided",
                  "No answer, refused", "None of the above"),
    "Other" = c("Others", "Other group", "Other group - Other group",
                "Other national family background - migration",
                "Other national/ ethnic identity", "Other, mixed origin",
                "Three groups or more", "Two nationalities, non swiss")
  )
)