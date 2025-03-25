data.spec <- list(
  file.name = "Latinobarometro_2023_Eng_Stata_v1_0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If elections where this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race you identify best with?"
  ),
  party.question.type = "PartyVote",
  country.format = 'spanish',
  country.dict = country.dict.es,
  field.def = c(
    "Party" = "P43STGBS_A",
    "Religion" = "S1",
    "Language" = "S15_A",
    "Ethnicity" = "S7",
    "Country" = "idenpa",
    "Year" = "numinves",
    "Weight" = "wt"
  )
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not registered", "Does not answer", "Don't know", "Vote void/blank", "Does not vote/none"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Does not answer", "Don't know"),
    "Other" = c("Other"),
    "No Religion" = c("Believer, not belong to the church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("Not applicable", "Does not answer", "Don't know"),
    "Other" = c("Other race")
  )
)
