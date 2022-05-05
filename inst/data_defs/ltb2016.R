data.spec <- list(
  file.name = "Latinobarometro2016Eng_v20170205.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If elections were held this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your mother tongue?",
    "Ethnicity" = "What race do you consider yourself to belong to?"
  ),
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
    "Missing" = c("No answer/Refused", "Don\u{00B4}t know"),
    "Other" = c("Others"),
    "No Religion" = c("Believer, not belonging to any church", "Agnostic", "Atheist", "None")
    
  ),
  Ethnicity = list(
    "Missing" = c("No answer/Refused", "Don\u{00B4}t know", "Not applicable"),
    "Other" = c("Other race")
  )
)
