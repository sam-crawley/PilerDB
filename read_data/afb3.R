data.spec <- list(
  file.name = "Divided/datasets/afrobarom/merged_r3_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "If a presidential election were held tomorrow, which party’s candidate would you vote for?",
    "Language" = "Which [Ghanaian/Kenyan/etc.] language is your home language?",
    "Religion" = "What is your religion, if any?",
    "Ethnicity" = "What is your tribe? You know, your ethnic or cultural group."
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q99",
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
    "Missing" = c("Don't know", "Missing data", "Refused", "Would not vote"),
    "Other" = c("independant", "Other", "Would vote for candidate and not party")
  ),
  Language = list(
    "Missing" = c("Missing"),
    "Other" = c("Other Northern Languages")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other"),
    "No Religion" = c("None", "Agnostic (Do not know if there is a God)", "Atheist (Do not believe in a God)")
  ),
  Ethnicity = list(
    "Other" = c("Related to Age",
                "Related to Gender",
                "Related to Religion",
                "Related to Class",
                "Related to Race",
                "National identity only",
                "Other",
                "Other Northern Languages",
                "Related to Occupation/Profession",
                "Related to Politics/Partizan"
    ),
    "Missing" = c("Don't know", "Refused", "Missing data", "Not asked")
  )
)
