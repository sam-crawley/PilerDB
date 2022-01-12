data.spec <- list(
  file.name = "Divided/datasets/latino/2013/Latinobarometro2013Eng.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race do you identify best with?"
  ),
  skip.countries = list(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "P22TGBSM",
    "Religion" = "S14",
    "Language" = "S7_A",
    "Ethnicity" = "S21",
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2015)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{00ED}a edad", "No preguntada"),
    "Other" = c("Otros (partidos nacionales y provinciales)")
  ),
  Language = list(
    "Missing" = c("Not asked"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("No answer", "Don't know"),
    "Other" = c("Others", "Believer"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("No answer/No data", "Don\u{00B4}t know", "Not applicable", "Not asked"),
    "Other" = c("Other race")
  )
)