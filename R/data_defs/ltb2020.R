data.spec <- list(
  file.name = "datasets/latino/2020/Latinobarometro_2020_Eng_Stata_v1_0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If elections where this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race you identify best with?"
  ),
  country.format = 'spanish',
  country.dict = country.dict.es,
  field.def = c(
    "Party" = "P50STGBS_A",
    "Religion" = "s10",
    "Language" = "s20_a",
    "Ethnicity" = "s12",
    "Country" = "idenpa",
    "Year" = "numinves",
    "Weight" = "wt"
  )
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Vote Nul/blank", "Does Not vote/none", "Does Not vote/none", "Not registered"),
    "Other" = c("Others")
  ),
  Religion = list(
    "Missing" = c("No answer", "Don\u{00B4}t know"),
    "Other" = c("Other"),
    "No Religion" = c("Believer, not belong to the church", "Agnostic", "Atheist", "None")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don\u{00B4}t know", "Not applicable"),
    "Other" = c("Other race")
  )
)
