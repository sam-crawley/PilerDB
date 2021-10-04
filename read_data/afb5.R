data.spec <- list(
  file.name = "Divided/datasets/afrobarom/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "If a presidential election were held tomorrow, which party’s candidate would you vote for?",
    "Language" = "Which language is your home language?",
    "Religion" = "What is your religion, if any?",
    "Ethnicity" = "What is your ethnic community, cultural group or tribe?"
  ),  
  skip.countries = list(
    no_party = c('Eswatini')
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q99",
    "Language" = "Q2",
    "Religion" = "Q98A",
    "Ethnicity" = "Q84",
    "Weight" = "combinwt",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% 
      mutate(Year = format(DATEINTR, format = "%Y")) %>%
      mutate(Religion = fct_recode(Religion, "Independent Protestant" = "Independent"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Don't know", "Refused to answer", "Undecided", "Would not vote"),
    "Other" = c("Others", "Best capable candidate/party with best policy", "Party of the mayor / husband / brother / village chief")
  ),
  Language = list(
    "Missing" = c("Missing", "Don't know"),
    "Other" = c("Others")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None", "Agnostic(Do not know if there is a God)", "Atheist(Do not believe in a God)")
  ),
  Ethnicity = list(
    "Other" = c("Related to regional origin (badio/sampadjudo)",
                "Related to political-partisan affiliation",
                "Related to age",
                "Related to gender",
                "Related to race",
                "Related to occupation",
                "Related to religion",
                "Related to class",
                "National identity only, or \"doesn't think of self in those terms\"",
                "Others"
    ),
    "Missing" = c("Missing", "Don't know", "Refused")
  )
)
