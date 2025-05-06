data.spec <- list(
  file.name = "AB7_ENG_Release_Version6.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party if any do you feel closest to?",
    "Religion" = "What is your religious denomination?",
    "Ethnicity" = "What is your ethnicity?"
  ),
  party.question.type = "Closest",
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q503A_1",
    "Language" = NA,
    "Religion" = "reltmp",
    "Ethnicity" = "Q1012B",
    "Weight" = "WT",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Use denominations rather than just Christian/Muslim
    coalese.vars(data, c("Q1012A_CHRISTIAN", "Q1012A_MUSLIM", "Q1012"), "reltmp")
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = format.Date(DATE, format = "%Y"))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No party", "Missing", "Don\u2019t know", "Refused to answer"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Missing", "Refused to answer"),
    "Other" = c("Other"),
    "No Religion" = c("No religion")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Don\u2019t know", "Refused to answer"),
    "Other" = c("Other")
  )
)
