data.spec <- list(
  file.name = "Divided/datasets/latino/2020/Latinobarometro_2020_Eng_Stata_v1_0.dta",
  file.type = 'dta',
  question.text = c(
  ),
  skip.countries = list(),
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
    "Missing" = c("No answer", "Don´t know"),
    "Other" = c("None", "Other", "Believer, not belong to the church", "Agnostic", "Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don´t know", "Not applicable"),
    "Other" = c("Other race")
  )
)