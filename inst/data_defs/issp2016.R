data.spec <- list(
  file.name = "ZA6900_v2-0-0.dta",
  file.type = 'dta',
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
    "Country" = "c_alphan",
    "Year" = "DATEYR",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY"), ends_with("RELIG")), ~if_else(.x == 990, NA, .x))) %>%
      mutate(across(ends_with("ETHN1"), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    data <- coalese.vars(data, str_subset(names(data), "RELIG$"), "relig")
    data <- coalese.vars(data, str_subset(names(data), "ETHN1$"), "ethn")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(DATEYR = if_else(DATEYR == 9999, NA, DATEYR))
    
    data
  },
  fixups = function(data) {
    # Remove unusable ethnicity questions
    data %>% mutate(Ethnicity = if_else(Country %in% c("Australia"), "Missing", Ethnicity))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "No answer incl. don't remember", "Refused", 
                  "No answer", "Blank vote", "Don't know, don't remember", "Does not know",
                  "Uncertain", "Don't know", "Don't know/don't remember", "Does not remember",
                  "Invalid balot", "Other (please specify)", "Invaild ballot"),
    "Other" = c("Other party", "Other Party", "Others", "Independent candidate",
                "Mixed vote (Candidates of several lists)", "Other Candidate")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know", "Refused to answer",
                  "Cannot say, don't know"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other non-Christian religions", "Other religious denomination",
                "Other religion", "Other religion",
                "Other Religions", "Other non-Christian", "Other Non-Christian",
                "Other Non-Christians", "Other non-Christian religion"),
    "No Religion" = c("Atheist", "No religion, none", "None", "No denomination or life stance organisation")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "None",
                  "NA incl. identifying with two groups (see DK_ETHN2)",
                  "No, do not belong to a minority",
                  "Yes, belong to a minority"),
    "Other" = c("Other, please specify", "Immigrants", "Mixed origin", 
                "Religious group", "World citizens", "Other option", 
                "Respondent mentions the province of birth", "Others",
                "3 ethnic groups or more", "None/ other group",
                "Respondent  mentions  lower territorial levels than province")
  )
)