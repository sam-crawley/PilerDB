data.spec <- list(
  file.name = "Latinobarometro_2004_datos_eng_v2014_06_27.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?"
  ),
  party.question.type = "PartyVote",
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
    "Missing" = c("No responde", "No sabe", "No documentado"),
    "Other" = c("Otros (partidos nacionales y provinciales)", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{c3}\u{ad}a edad")
  ),
  Language = list(
    "Missing" = c("Not applicable", "Not asked"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u00C2\u00B4t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  )
)
