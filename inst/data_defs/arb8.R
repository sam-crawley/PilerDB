data.spec <- list(
  file.name = "ArabBarometer_WaveVIII_English_v1.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party if any do you feel closest to?",
    "Religion" = "What is your religious denomination?"
  ),
  party.question.type = "Closest",
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q503A_2",
    "Language" = NA,
    "Religion" = "reltmp",
    "Ethnicity" = NA,
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
  )
)
