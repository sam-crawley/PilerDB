data.spec <- list(
  file.name = "datasets/latino/2015/Latinobarometro_2015_Eng.dta",
  file.type = 'dta',
  question.text = c(
      "Party" = "If elections were held this Sunday, which party would you vote for?",
      "Religion" = "What is your religion?",
      "Language" = "What is your native language?",
      "Ethnicity" = "What ethnicity or race do you identify best with?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "P23TGBSM",
    "Religion" = "S16",
    "Language" = "S5A",
    "Ethnicity" = "S23",
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
    "Missing" = c("No responde", "No sabe", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{00ED}a edad"),
    "Other" = c("Otros (partidos nacionales y provinciales)")
  ),
  Language = list(
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("No answer", "Don't know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don\u{00B4}t know", "Not applicable"),
    "Other" = c("Other race")
  )
)
