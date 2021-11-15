data.spec <- list(
  file.name = "Divided/datasets/afrobarom/merged_r6_data_2016_36countries2.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "If a presidential election were held tomorrow, which party’s candidate would you vote for?",
    "Language" = "Which language is your home language?",
    "Religion" = "What is your religion, if any?",
    "Ethnicity" = "What is your ethnic community, cultural group or tribe?"
  ),  
  skip.countries = list(
    no_party = c("Eswatini")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q99",
    "Language" = "Q2",
    "Religion" = "Q98A",
    "Ethnicity" = "Q87",
    "Weight" = "Combinwt",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(DATEINTR, format = "%Y")) %>%
      mutate(Religion = fct_recode(Religion, "Independent Protestant" = "Independent"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Don't know", "Not Asked in this Country", "Refused to answer", "Not yet decided-MLW", "Would not vote"),
    "Other" = c("Vote for a party programme of project", "Other")
  ),
  Language = list(
    "Missing" = c("Missing", "Don't know")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused", "NOT ASKED IN THIS COUNTRY"),
    "Other" = c("Other", "None", "Atheist", "Agnostic")
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
    "Missing" = c("Missing", "Don't know", "Refused to answer", "Not asked in country")
  )
)
