data.spec <- list(
  file.name = "Latinobarometro_2007_datos_eng_v2014_06_27.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race do you identify best with?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  field.def = c(
    "Party" = "p64st",
    "Religion" = "s4",
    "Language" = "s22",
    "Ethnicity" = "s9",
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2007)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe"),
    "Other" = c("Otros (partidos nacionales y provinciales)", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{00ED}a edad")
  ),
  Language = list(
    "Missing" = c("Not applicable", "Not asked"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u{00B4}t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u{00B4}t know"),
    "Other" = c("Other race")
  )
)
