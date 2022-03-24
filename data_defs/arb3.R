data.spec <- list(
  file.name = "datasets/arab/Wave 3/ABIII_English.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which of the existing parties is closest to representing your political, social and economic aspirations?",
    "Religion" = "What is your religious denomination?",
    "Language" = "What is your first language?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q503",
    "Language" = "q1019_1",
    "Religion" = "q1012a",
    "Ethnicity" = "q2001ir",
    "Weight" = "wt",
    "Country" = "country",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = if_else(is.na(date), "2014", format(date, format = "%Y")))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No party represents my aspirations", "Don't know", "Refuse", "Missing"),
    "Other" = c("Others")
  ),
  Language = list (
    "Missing" = c("Refuse", "Missing")
  ),
  Ethnicity = list(
    "Missing" = c("Don't know", "Refuse")
  ),
  Religion = list(
    "Missing" = c("Refuse")
  )
)
