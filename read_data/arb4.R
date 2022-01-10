data.spec <- list(
  file.name = "Divided/datasets/arab/Wave 4/ABIV_English.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which of the existing parties is closest to representing your political, social and economic aspirations?",
    "Religion" = "What is your religious denomination?",
    "Language" = "What is your first language?"
  ),  
  skip.countries = list(
    no_group = c("Egypt", "Jordan", "Palestinian Territories")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q503",
    "Language" = "q1019a",
    "Religion" = "q1012a",
    "Ethnicity" = NA,
    "Weight" = "wt",
    "Country" = "country",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2017)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Are not allowed to vote by regime", "Don't know (Do not read)", "Decline to answer (Do not read)", "No party", "Missing")
  ),
  Language = list (
    "Missing" = c("Don't know (Do not read)", "Decline to answer (Do not read)")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know (Do not read)", "Decline to answer (Do not read)"),
    "No Religion" = c("None")
  )
)
