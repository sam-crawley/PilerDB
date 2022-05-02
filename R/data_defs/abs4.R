data.spec <- list(
  file.name = "W4_v15_merged20181211_release.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which parties (or candidates for president if it was presidential race) did you vote for?",
    "Religion" = "What is your religion?",
    "Ethnicity" = "What's your racial (ethnic) background?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q34",
    "Religion" = "se6",
    "Language" = NA,
    "Ethnicity" = "se11a",
    "Country" = "country",
    "Year" = "year",
    "Weight" = "w"
  ),
  fixups = function(data) {
    # Munge Singapore year (spans 2 years)
    data %>% mutate(Year = ifelse(Country == "Singapore" & Year == 2015, 2014, Year))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Cannot recall", "Invalid vote", "Do not understand the question",
                  "Missing", "Can't choose", "Decline to answer"),
    "Other" = c("Other party", "Independent", "Other", "Parties in the respective region", "Individual Candidate")
  ),
  Religion = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer"),
    "Other" = c("Other"),
    "No Religion" = c("None")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer"),
    "Other" = c("Others", "Other")
  )
)