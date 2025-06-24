data.spec <- list(
  file.name = "Latinobarometro_2000_datos_eng_v2014_06_27.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were elections tomorrow, which party would you vote for?",
    "Religion" = "What is your religion?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  field.def = c(
    "Party" = "P54ST",
    "Religion" = "P76ST",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2000)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Voto secreto", "Vota por personas, no por partidos", "No documentado"),
    "Other" = c("Otros (partidos nacionales y provinciales)", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{00ED}a edad")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u{00B4}t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  )
)
