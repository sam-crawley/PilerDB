data.spec <- list(
  file.name = "ZA8000_v2-0-0.dta",
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
    "Weight" = "WEIGHT_COM"
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
      mutate(relig = str_replace(relig, "Yes, consider to belong: ", "")) %>%
      mutate(across(c(prty, relig, ethn), ~str_replace(.x, "^\\d+\\.\\s+", "")))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "Invalid ballot (No second vote)"),
    "Other" = c("Other party", "Independent candidate", "Other", "Independent", "Other candidate", "Other Party",
                "Mixed vote (Candidates of several lists)")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Yes, consider to belong to a religion but: No answer as to which religion",
                "Other", "Other non-christians", "Other religions", "Other Asian religions",
                "Other non-Christian religions", "Other Non-Christians", "Other religious denomination",
                "Other religion", "Other non-Christian Denomination",
                "Other Non-Christian", "Inter- or Non-denominational", "Other religion", "Other Religions",
                "Other Asian Religions", "Other (please specify)", "Other (specify)"),
    "No Religion" = c("Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No ethnic affiliation", "None", "No, not belonging to an indigenous group",
                  "I do not consider as belonging to a particular group",
                  "Other North African and Middle Eastern"),
    "Other" = c("Both parents born in Austria", "Both parents born abroad", "One parent born in abroad",
                "Danish group", "Religious group", "Migrants group", "Returnees", "Others", "Other North American",
                "Other countries", "Other (please specify)", "Other non western",
                "Other western", "None or other group")
  )
)