data.spec <- list(
  file.name = "ZA5800_v3-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "PartyVote",
  country.party.question.type = c(
    "Chile" = "Closest",
  ),
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "BE-FLA" = "Belgium",
    "BE-WAL" = "Belgium", 
    "DE-E" = "Germany",
    "DE-W" = "Germany"
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
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty)) %>%
      mutate(DATEYR = if_else(DATEYR == 9999, NA, DATEYR))
    
    data
  },
  fixups = function(data) {
    # Remove unusable ethnicity questions
    data %>% mutate(Ethnicity = 
                      if_else(Country %in% c("Iceland", "Sweden", "Australia", "France", "Norway"), "Missing", Ethnicity))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "Refused", "No answer", "Vote blank, not valid",
                  "Don't remember", "Don't know", "Blank vote", "None", "Do not know",
                  "I would not vote", "Null vote", "Other party, specify", 
                  "Does not know", "Doesn't remember", "No party affiliation, preference"),
    "Other" = c("Other party", "Other Party", "Mixed vote (Candidates of several lists)",
                "Independent", "Other answer", "Vote for a certain person, not for a specific party",
                "Other Parties", "Other Party - Communist party / KSS", "Other Party: UKIP, BNP/NF, Scottish Socialist/Respect/Social")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other religious denomination", "Other religion", "Other religion",
                "Other Religions", "Other non-Christian", "Other (specify)",
                "Other non-Christian Religions", "Other Non-Christian"),
    "No Religion" = c("No religion", "No religious affiliation")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "No answer, Don't know",
                  "None of these"),
    "Other" = c("Other, mixed origin", "[Other] Developing world",
                "Other national/ethnic identity", "Two nationalities, non Swiss")
  )
)