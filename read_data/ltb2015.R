data.spec <- list(
  file.name = "Divided/datasets/latino/2015/Latinobarometro_2015_Eng.dta",
  file.type = 'dta',
  question.text = c(
  ),
  skip.countries = list(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "P23TGBSM",
    "Religion" = "S16",
    "Language" = "S5A",
    "Ethnicity" = "S23",
    "Country" = "idenpa",
    "Year" = "numinves",
    "Weight" = "wt"
  )
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No tenía edad"),
    "Other" = c("Otros (partidos nacionales y provinciales)")
  ),
  Language = list(
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("No answer", "Don't know"),
    "Other" = c("None", "Others", "Believer, not belonging to any church", "Agnostic", "Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don´t know", "Not applicable"),
    "Other" = c("Other race")
  )
)