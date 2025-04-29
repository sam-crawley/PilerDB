data.spec <- list(
  file.name = "ZA3680.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  country.custom = c(
    "Northern Ireland" = "United Kingdom",
    "Southafrica" = "South Africa",
    "Germany-East" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "V3",
    "Year" = "Year",
    "Weight" = "weight"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY")), ~if_else(.x == 0, NA, .x)))
    
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    
    data <- data %>%
      mutate(Year = 2001)
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote", "None", 
                   "Would not vote;no party preference", "Would not vote;not eligible",
                   "Would not vote;No party preference", "No party preference;wouldnt vote", 
                   "No preference, not vote", "None of them", "Voted blank", "Blank",
                   "(NONE)", "No vote", "No vote;no party preference", "Dont remember",
                   "Dont know", "No party, no preference"),
    "Other" = c("Other party", "Other answer", "Other Party", "Far right", "Far left",
                "Other Answer (specify)", "Other list", "Other Party (specify)")
  ),
  Religion = list(
    "Missing" = c("No denomination given"),
    "Other" = c("Other not classified", "Other non-christian"),
    "No Religion" = c("None")
  )
)