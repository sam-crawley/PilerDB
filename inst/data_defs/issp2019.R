data.spec <- list(
  file.name = "ZA7600_v3-0-0.dta",
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
    "Missing" = c("Invalid ballot", "None"),
    "Other" = c("Other party", "Independent candidate", "Other candidate", "Other Party",
                "Mixed vote (Candidates of several lists)", "A different party",
                "Other party or political organization")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other non-Christian religions", "Other religious denomination",
                "Other religion", "Inter- or Non-denominational", "Other religion", 
                "Other Religions", "Other non-Christian denomination", "Other non-Christian",
                "Other Religion", "Other Non-Christians", "Other non-Christian religion",
                "No answer to which religion or denomination", "Other non-Christian Denomination"),
    "No Religion" = c("Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("None", "No group", "None of the these"),
    "Other" = c("Both parents born in Austria", "Both parents born abroad", "One parent born abroad",
                "Danish group", "Religious group", "Migrants group", "None or other group", 
                "Other, please specify", "Other North African and Middle Eastern",
                "Foreigner", "Returning migrants", "Immigrants", "Mixed origin",
                "Other ethnic groups", "Other, specify")
  )
)