data.spec <- list(
  file.name = "ZA7650_v2-0-0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
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
      mutate(across(c(ends_with("PRTY"), ends_with("RELIG"), ends_with("ETHN1")), ~if_else(.x == -2, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    data <- coalese.vars(data, str_subset(names(data), "RELIG$"), "relig")
    data <- coalese.vars(data, str_subset(names(data), "ETHN1$"), "ethn")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(across(c(prty, relig, ethn), ~if_else(str_detect(.x, "^\\-\\d"), "Missing", .x))) %>%
      mutate(relig = if_else(str_detect(relig, "^0\\."), "No Religion", relig)) %>%
      mutate(relig = str_replace(relig, "Yes, consider to belong to: ", "")) %>%
      mutate(across(c(prty, relig, ethn), ~str_replace(.x, "^\\d+\\.\\s+", "")))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "Blank vote", "Voted blanc or null", "Uncertain"),
    "Other" = c("Other party", "Independent candidate", "Other candidate", "Other Party",
                "Mixed vote (Candidates of several lists)", "A different party", "Other (Please specify)")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other non-Christian religions", "Other religious denomination",
                "Other religion", "Other Non-Christian", "Inter- or Non-denominational", "Other religion", 
                "Other Religions", "Other Asian Religions", "Other (specify)",
                "Other non-Christian denomination", "Other non-Christian", "Other religion (specify)",
                "No answer to which religion or denomination"),
    "No Religion" = c("Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("None", "Other North African and Middle Eastern", "No group", "No particular group"),
    "Other" = c("Both parents born in Austria", "Both parents born abroad", "One parent born abroad",
                "Danish group", "Religious group", "Migrants group", "Others", "Other North American",
                "None or other group", "Other (former nations, specific regions)",
                "Respondent mentions the country of birth and/ or nationality", "World citizens",
                "Other Option", "Respondent mentions lower territorial levels than province (town, island, etc.)",
                "Other, please specify")
  )
)