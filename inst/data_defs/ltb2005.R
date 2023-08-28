data.spec <- list(
  file.name = "Latinobarometro_2005_datos_eng_v2014_06_27.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?"
  ),
  country.format = 'country.name',
  country.custom = c(
    "Rep\u00C3\u00BAblica Dominicana" = "Dominican Republic"
  ),
  field.def = c(
    "Party" = "p48st",
    "Religion" = "s2",
    "Language" = "s18",
    "Ethnicity" = NA,
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2005)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "GT: No documentado - 923", "GT: No documentado - 924", "GT: No documentado - 925",
                  "GT: No documentado - 926", "GT: No documentado - 927", "GT: No documentado - 928", "GT: No documentado - 929",
                  "GT: No documentado - 930", "NI: No documentado - 1235", "NI: No documentado - 1236", "NI: No documentado - 1237",
                  "NI: No documentado - 1238"),
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
