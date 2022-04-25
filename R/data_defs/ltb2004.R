data.spec <- list(
  file.name = "datasets/latino/2004/Latinobarometro_2004_datos_eng_v2014_06_27.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "p30st",
    "Religion" = "p90st",
    "Language" = "s13",
    "Ethnicity" = NA,
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2004)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe"),
    "Other" = c("Otros (partidos nacionales y provinciales)", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{c3}\u{ad}a edad")
  ),
  Language = list(
    "Missing" = c("Not applicable", "Not asked"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "DonÂ´t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  )
)
