data.spec <- list(
  file.name = "Divided/datasets/latino/2017/Latinobarometro2017Eng_v20180117.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party would you vote for if elections were next Sunday?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race you identify best with?"
  ),
  skip.countries = list(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "P16STGBS",
    "Religion" = "S9",
    "Language" = "S24_A",
    "Ethnicity" = "S10",
    "Country" = "idenpa",
    "Year" = "numinves",
    "Weight" = "wt"
  )
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No answer/Refused", "Don\u{00B4}t know", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No ten\u{00ED}a edad"),
    "Other" = c("Otros (partidos nacionales y provinciales)")
  ),
  Language = list(
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("No answer/Refused", "Don\u{00B4}t know"),
    "Other" = c("None", "Others", "Believer, not belonging to any church", "Agnostic", "Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No answer/Refused", "Don\u{00B4}t know", "Not applicable"),
    "Other" = c("Other race")
  )
)