data.spec <- list(
  file.name = "Divided/data/afrobarom/merged_r4_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  skip.countries = c(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q86",
    "Language" = "Q3",
    "Religion" = "Q90",
    "Ethnicity" = "Q79",
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
    "Missing" = c("Missing", "Don't know", "Refused to answer", "Not Applicable"),
    "Other" = c("Others")
  ),
  Language = list(
    "Missing" = c("Missing"),
    "Other" = c("Others")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None", "Agnostic(Do not know if there is a God)", "Atheist(Do not believe in a God)")
  ),
  Ethnicity = list(
    "Other" = c("Related to regional origin (badio/sampadjudo)",
                "Related to political-partisan affiliation",
                "National ID only or 'doesn't think of self in those terms'",
                "Related to Age",
                "Related to Gender",
                "Related to Occupation",	
                "Related to Religion",
                "Related to Class",
                "Related to Race",
                "Relacionado com o estado de espirito",
                "Others"
    ),
    "Missing" = c("Missing", "Don't know", "Refused")
  )
)
