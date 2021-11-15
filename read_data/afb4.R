data.spec <- list(
  file.name = "Divided/datasets/afrobarom/merged_r4_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "If a presidential election were held tomorrow, which party’s candidate would you vote for?",
    "Language" = "Which [Ghanaian/Kenyan/etc.] language is your home language?",
    "Religion" = "What is your religion, if any?",
    "Ethnicity" = "What is your tribe? You know, your ethnic or cultural group."
  ),  
  skip.countries = c(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q97",
    "Language" = "Q3",
    "Religion" = "Q90",
    "Ethnicity" = "Q79",
    "Weight" = "Combinwt",
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
    "Missing" = c("Missing", "Don't know", "Refused to answer", "Would not vote"),
    "Other" = c("Others", "Any winning party", "Any other than LCD", "Would vote for candidate and not party",
                "An independent candidate", "An opposition candidate", "A person who can help the country/address problems/change th")
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
