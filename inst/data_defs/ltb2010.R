data.spec <- list(
  file.name = "Latinobarometro_2010_datos_eng_v2014_06_27.dta",
  file.type = 'dta',
  file.encoding = "latin1",  
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race do you identify best with?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  field.def = c(
    "Party" = "P29ST",
    "Religion" = "S9",
    "Language" = "P79_A",
    "Ethnicity" = "S20",
    "Country" = "idenpa",
    "Year" = NA,
    "Weight" = "wt"
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2010)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u00C2\u00B4t know"),
    "Other" = c("Other (national and regional parties)", "Null vote/Blank vote", "Do not vote/None", "Not registered/No legal age",
                "CL: Vota por personas")
  ),
  Language = list(
    "Missing" = c("Not applicable"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u00C2\u00B4t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("Not applicable", "No answer/Refused", "Don\u00C2\u00B4t know"),
    "Other" = c("Other race")
  )
)
