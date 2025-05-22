data.spec <- list(
  file.name = "ZA7570_v2-1-0.dta",
  file.type = 'dta',
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
      mutate(across(c(prty, relig), ~if_else(str_detect(.x, "^\\-\\d"), "Missing", .x))) %>%
      mutate(relig = if_else(str_detect(relig, "^0\\."), "No Religion", relig)) %>%
      mutate(prty = if_else(str_detect(prty, "^0\\."), "Missing", prty)) %>%
      mutate(prty = str_replace(prty, "Yes, ", "")) %>%
      mutate(across(c(prty, relig, ethn), ~str_replace(.x, "^\\d+\\.\\s+", ""))) %>%
      mutate(DATEYR = if_else(DATEYR == 9999, NA, DATEYR))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "but can't remember", "Refused", "Don't know, don't remember",
                  "No answer", "Blank vote", "Don't know, can't remember", "Don't know",
                  "cast an invalid ballot"),
    "Other" = c("Other party", "Other candidate", "Other Party",
                "Mixed vote (Candidates of several lists)")
  ),
  Religion = list(
    "Missing" = c("Refused", "Don't know", "No answer", "Information insufficient"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other non-Christian religions", "Other religious denomination",
                "Other religion", "Inter- or Non-denominational", "Other religion", 
                "Other Religions", "Other non-Christian denomination", "Other non-Christian",
                "Other Non-Christians", "Other non-Christian religion"),
    "No Religion" = c("Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "None of these", "No", "None",
                  "NA, respondent identifies with 2 groups (see DK_ETHN2)", "Dont know"),
    "Other" = c("Both parents born in Austria", "Both parents born abroad", "One parent born abroad",
                "Other, please specify", "Other North African and Middle Eastern",
                "Immigrants", "Mixed origin", "Other regional groups", "None or other group",
                "Other ethnic groups", "Other, specify", "Other ethnic group",
                "Religious group", "Migrants group", "Respondent mentions the country of birth and/or nationality",
                "World citizens", "Other option", "Respondent mentions the province of birth", "Yes")
  )
)