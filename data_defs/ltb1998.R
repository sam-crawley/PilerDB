data.spec <- list(
  file.name = "datasets/latino/1998/Latinobarometro_1998_datos_english_v2014_06_27.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If there were elections tomorrow, which party would you vote for?",
    "Religion" = "What is your religion?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "sp53",
    "Religion" = "sp80",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "pondera"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 1998)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Voto secreto", "Vota por personas, no por partidos"),
    "Other" = c("Otros (partidos nacionales y provinciales)", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{c3}\u{ad}a edad")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "DonÂ´t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  )
)
