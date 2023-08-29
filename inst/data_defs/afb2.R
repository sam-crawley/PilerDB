data.spec <- list(
  file.name = "merged_r2_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Do you feel close to any particular political party or political organization? If so, which party or organization is that?",
    "Language" = "Which language is your home language?",
    "Religion" = "What is your religion, if any?"
  ),
  party.question.type = "Closest",
  country.format = 'country.name',
  field.def = c(
    "Party" = "q87a",
    "Language" = "q83",
    "Religion" = "q85",
    "Ethnicity" = NA,
    "Weight" = "combinwt",
    "Country" = "country",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(dateintr, format = "%Y"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Don't know", "No, not close to any party", "Missing", "Refused to answer"),
    "Other" = c("Independent", "Other")
  ),
  Language = list(
    "Missing" = c("Missing"),
    "Other" = c("Other", "Other Western languages", "Other Eastern languages", "Other Northern languages")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other"),
    "No Religion" = c("None", "Agnostic (Do not know if there is a God)", "Atheist (Do not believe in a God)")
  )
)
