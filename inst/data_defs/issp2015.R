data.spec <- list(
  file.name = "ZA6770_v2-1-0.dta",
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
    "Missing" = c("Invalid ballot", "Refused", "Don\u0092t know", "Don\u0092t know, don\u0092t remember",
                  "No answer", "Blank vote", "Don't know, don't remember", "Does not know",
                  "Uncertain", "Don't know/don't remember", "Does not remember",
                  "Other (please specify)",
                  "Hard to say, don't remember"),
    "Other" = c("Other party", "Other Party", "Others", "Independent candidate",
                "Mixed vote (Candidates of several lists)", "Other Candidate")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know", "Refused to answer",
                  "Cannot say, don't know", "Does not know if related to any religion, church or denomina",
                  "Can't choose"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other religious denomination", "Other religion", "Other religion",
                "Other Religions", "Other non-Christian", "Other Non-Christian",
                "Other Non-Christians", "Other non-Christian religion", "Other non-Christian denomination",
                "Other non-Christian Religions", "Other not Christian",
                "Not related to any religion, church or denomination",
                "My own religion"),
    "No Religion" = c("Atheist", "No religion, none", "None", "No religious denomination or life stance organisation",
                      "No religion, atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "None", "Dont know",
                  "NA incl. identifying with two groups (see DK_ETHN2)",
                  "NA/ inadequately described, no group", "No ethnic group",
                  "No, not belonging to a national minority", "Non-classifiable groups",
                  "None of these", "None/other group", "Not belonging to an indigenous group"),
    "Other" = c("Other, please specify", "Immigrants", "Mixed origin", 
                "Religious group", "World citizens", "Other option", 
                "Respondent mentions the province of birth", "Others",
                "Respondent  mentions  lower territorial levels than province",
                "Other ethnic group", "Three ethnic groups or more",
                "Three or more groups mentioned", "Yes, belonging to a national minority")
  )
)