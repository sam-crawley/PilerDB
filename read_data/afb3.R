data.spec <- list(
  file.name = "Divided/data/afrobarom/merged_r3_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  skip.countries = list(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q86",
    "Language" = "q3",
    "Religion" = "q91",
    "Ethnicity" = "q79",
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
    "Missing" = c("Don't know", "Missing data", "Not applicable", "Refused"),
    "Other" = c("independant", "Other")
  ),
  Language = list(
    "Missing" = c("Missing"),
    "Other" = c("Other Northern Languages")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None", "Agnostic (Do not know if there is a God)", "Atheist (Do not believe in a God)")
  ),
  Ethnicity = list(
    "Other" = c("Related to Age",
                "Related to Gender",
                "Related to Religion",
                "Related to Class",
                "Related to Race",
                "National identity only",
                "Other",
                "Other white/ European",
                "Other Northern Languages"
    ),
    "Missing" = c("Don't know", "Refused", "Missing data", "Not asked")
  )
)
