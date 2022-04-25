data.spec <- list(
  file.name = "datasets/arab/Wave 2/ABII_English.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which of the existing parties is closest to representing your political, social and economic aspirations?",
    "Religion" = "What is your religion?",
    "Language" = "What is your first language?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q503",
    "Language" = "q10191",
    "Religion" = "q1012",
    "Ethnicity" = "ir2001",
    "Weight" = "wt",
    "Country" = "country",
    "Year" = NA
  ),
  fixups = function(data) {
    data <- data %>% mutate(Year = 2012)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\d+\\.\\s*")
    }
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("declined to answer", "don't know", "unspecified", "not specified", "don't remember", "none", "no party represents my aspirations")
  ),
  Language = list (
    "Missing" = c("declined to answer", "i don?t know", "there is no second language", "unspecific answer", "missing")
  ),
  Ethnicity = list(
    "Missing" = c("declined to answer", "i don?t know", "missing")
  ),
  Religion = list(
    "Missing" = c("declined to answer", "missing", "unspecific answer")
  )
)
