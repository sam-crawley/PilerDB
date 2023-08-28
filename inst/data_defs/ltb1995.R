data.spec <- list(
  file.name = "Latinobarometro_1995_data_english_v2014_06_27.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If there were elections tomorrow, which party would you vote for?",
    "Religion" = "What is your religion?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "p33",
    "Religion" = "p9",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "pais",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 1995)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Voto secreto", "Vota por personas, no por partidos", "No documentado", "No aplica"),
    "Other" = c("Otros (partidos nacionales y provinciales)", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{c3}\u{ad}a edad")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u00C2\u00B4t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  )
)
