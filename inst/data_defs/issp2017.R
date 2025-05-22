data.spec <- list(
  file.name = "ZA6980_v2-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "PartyVote",
  country.party.question.type = c(
    "France" = "PresPartyVote",
    "Mexico" = "PresPartyVote",
    "Taiwan" = "PresPartyVote",
    "United States" = "PresPartyVote"
  ),
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
    data %>% mutate(Ethnicity = if_else(Country %in% c("Australia", "Austria"), "Missing", Ethnicity))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "No answer incl. don't remember", "Refused", "Don\u0092t know, can\u0092t remember",
                  "No answer", "Blank vote", "Don't know, can't remember;  No answer", "Can\u0092t remember, information insufficient",
                  "Don\u0092t know, don\u0092t remember", "Don't know, don't remember", "Does not know",
                  "Uncertain"),
    "Other" = c("Other party", "Other Party", "Others", "A different party", "Independent candidate",
                "Mixed vote (Candidates of several lists)", "Other Candidate")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Information insufficient", "Don\u00b4t know"),
    "Other" = c("Other", "Other religions", "Other Asian religions", "Other not Christian (e.g. Jewish, Islamic)",
                "Other non-Christian religions", "Other religious denomination",
                "Other religion", "Other religion", "Other (specify)",
                "Other Religions", "Other non-Christian", "Other Non-Christian",
                "Other Non-Christians", "Other non-Christian religion"),
    "No Religion" = c("Atheist", "No religion, none", "No religion")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "None", "Dont know",
                  "NA incl. identifying with two groups (see DK_ETHN2)",
                  "None / other group", "No, do not belong to a minority",
                  "Yes, belong to a minority"),
    "Other" = c("Other, please specify", "Immigrants", "Mixed origin", 
                "Religious group", "Respondent mentions the country of birth and/or nationality",
                "World citizens", "Other option", "Respondent mentions the province of birth",
                "Respondent mentions lower territorial levels than province (",
                "Not belonging to an indigenous group", "Others")
  )
)