data.spec <- list(
  file.name = "Latinobarometro_2011_eng.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race do you identify best with?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "P38ST",
    "Religion" = "S18",
    "Language" = "S11_A",
    "Ethnicity" = "S27",
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2011)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{00ED}a edad", "No preguntada"),
    "Other" = c("Otros (partidos nacionales y provinciales)")
  ),
  Language = list(
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("No answer", "Don\u{00B4}t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don\u{00B4}t know"),
    "Other" = c("Other race")
  )
)
