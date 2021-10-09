data.spec <- list(
  file.name = "Divided/datasets/latino/2016/Latinobarometro2016Eng_v20170205.dta",
  file.type = 'dta',
  question.text = c(
  ),
  skip.countries = list(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "P15STGBS",
    "Religion" = "S8",
    "Language" = "S17A",
    "Ethnicity" = "S9",
    "Country" = "idenpa",
    "Year" = "numinves",
    "Weight" = "wt"
  )
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No answer", "Don't know", "Null/Blank Ballot", "Didn't vote/None", "Not registered/Not age of voting"),
    "Other" = c("Others (national and provincial parties)")
  ),
  Language = list(
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("No answer/Refused", "Don´t know"),
    "Other" = c("None", "Others", "Believer, not belonging to any church", "Agnostic", "Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No answer/Refused", "Don´t know", "Not applicable"),
    "Other" = c("Other race")
  )
)