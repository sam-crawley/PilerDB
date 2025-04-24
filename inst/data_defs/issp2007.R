data.spec <- list(
  file.name = "ZA4850_v2-0-0.dta",
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
    "FLA" = "Belgium"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = "ethnic",
    "Country" = "V5.mod",
    "Year" = "Year",
    "Weight" = "weight"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY")), ~if_else(.x == 0, NA, .x))) %>%
      mutate(V5.mod = str_extract(as.character(haven::as_factor(V5)), "^(.+)\\-", group = 1))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(Year = 2007) %>%
      mutate(prty = if_else(V5.mod == "US", "Missing", prty))
    
    data
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Uncertain", "Would not vote,no party preference",
                  "Vote for a certain person not for a party",
                  "Against all/ threw out/ damaged voting paper"),
    "Other" = c("Other party", "Other answer", "Other Party", "Other, center, center-right",
                "Other, left", "independent")
  ),
  Religion = list(
    "Missing" = c("No answer"),
    "Other" = c("Other Religions", "Other non-Christian Religions"),
    "No Religion" = c("No religion")
  ),
  Ethnicity = list(
    "Missing" = c(),
    "Other" = c("Other,mixed origin")
  )
)