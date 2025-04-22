data.spec <- list(
  file.name = "ZA6670_v2-0-0.dta",
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
    data %>% mutate(Ethnicity = if_else(Country %in% c("Australia", "Austria", "Iceland"), "Missing", Ethnicity))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "Refused", "No answer, refused",
                  "No answer", "Blank vote", "Don't know, don't remember",
                  "Don't Know", "Hard to say, don't remember", "Voted blank",
                  "No answer, don't know", "Blank ballot", "Don't remember",
                  "No party affiliation", "Doesn't remember", "Don't know",
                  "Don't remember/ Don't know",
                  "Voted in election in country other than Australia"),
    "Other" = c("Other party", "Other Party", "Independent candidate",
                "Mixed vote (Candidates of several lists)", " Other Party",
                "Other candidate", "Other party, specify")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know",
                  "Does not know if related to any religion, church or denomination",
                  "Not belonging to any religion"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other religious denomination", "Other religion", "Other religion",
                "Other Religions", "Other non-Christian", "Other Non-Christian",
                "Other Non-Christians", "Other non-Christian Religions",
                "Not related to any religion, church or denomination",
                "My own religion", "Other Religion", "Other not Christian (eg. Jewish, Islamic)",
                "Another non-Christian religion"),
    "No Religion" = c("Atheist", "No denomination or life stance organisation",
                      "No religion")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "Dont know",
                  "Non-classifiable groups", "None of these",
                  "Can't say", "No answer/ inadequately described",
                  "None/ other group", "Undecided"),
    "Other" = c("Other, please specify", "Immigrants", "Mixed origin", 
                "World citizens", "Other option", 
                "Others", "Three or more groups mentioned",
                "3 ethnic groups or more",
                "Identifying with two groups (see DK_ETHN2)",
                "Other nationa/ ethnic identity", "Other origin",
                "Other, mixed", "Reference to lower territorial levels than province (town, island...)",
                "Reference to the country of birth", "Reference to the province of birth",
                "The World (NO COUNTRY - GEOGRAPHICAL AREA)")
  )
)