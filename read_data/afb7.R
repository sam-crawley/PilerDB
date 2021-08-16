# Q99 - Party
# Q98 - Religion
# Q2A - Language
# Q84 - Ethnicity

data.spec <- list(
  file.name = "Divided/data/afrobarom/r7_merged_data_34ctry.release.sav",
  file.type = 'sav',
  skip.countries = c("Eswatini"),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q99",
    "Language" = "Q2A",
    "Religion" = "Q98",
    "Ethnicity" = "Q84",
    "Weight" = "Combinwt",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(DATEINTR, format = "%Y"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Would not vote", "Refused", "Don't know", "Not asked in the country")
  ),
  Language = list(
    "Missing" = c("Missing", "Refused To Answer", "Don't know")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None", "Atheist", "Agnostic")
  ),
  Ethnicity = list(
    "Other" = c("Other", "Doesn\u2019t think of self in those terms",
                "Related to regional origin (badio/sampadjudo)",
                "Related to Gender",
                "Related to Religion",
                "Related to Race",
                "Related to political-partisan affiliation",
                "Related to social groups (Foros, Angulares, Cabo-verdianos, Principenses)",
                "Related to age",
                "Related to gender",
                "Related to the job",
                "Related to religian",
                "Related to social classes",
                "Related to race",
                "Related to political party affiliation"),
    "Missing" = c("Missing", "Not asked in the country", "Refused", "Don't know")
  )
)
