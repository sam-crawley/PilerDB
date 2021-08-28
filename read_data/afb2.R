data.spec <- list(
  file.name = "Divided/data/afrobarom/merged_r2_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  skip.countries = c(),
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
    "Other" = c("Other", "None", "Agnostic (Do not know if there is a God)", "Atheist (Do not believe in a God)")
  )
)
