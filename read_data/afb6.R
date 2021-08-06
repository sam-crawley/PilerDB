data.spec <- list(
  file.name = "Divided/data/afrobarom/merged_r6_data_2016_36countries2.sav",
  file.type = 'sav',
  skip.countries = c("Egypt", "Eswatini"),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q90B",
    "Language" = "Q2",
    "Religion" = "Q98A",
    "Ethnicity" = "Q87",
    "Weight" = "withinwt",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(DATEINTR, format = "%Y"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Don't know", "Not Asked in this Country", "Not applicable", "Refused to answer")
  ),
  Language = list(
    "Missing" = c("Missing", "Don't know")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None", "Atheist")
  ),
  Ethnicity = list(
    "Other" = c("Other", 
                "Related to regional origin (badio/sampadjudo)",
                "Related to political-partisan affiliation",
                "Related to age",
                "Related to gender",
                "Related to race",
                "Related to class",
                "Related to occupation",
                "Related to religion",
                "Related to regional origin (Foros, Angulares, Cabo-verdianos)",
                "National identity only, or 'doesnt think of self in those terms'"
            ),
    "Missing" = c("Missing", "Don't know", "Refused to answer")
  )
)
