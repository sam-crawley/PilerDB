data.spec <- list(
  file.name = "afrobarometer_release-dataset_merge-34ctry_r8_en_2023-03-01.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "If a presidential election were held tomorrow, which party's candidate would you vote for?",
    "Language" = "What is the primary language you speak in your home now?",
    "Religion" = "What is your religion, if any?",
    "Ethnicity" = "What is your ethnic community, cultural group or tribe?"
  ),
  party.question.type = "PresPartyVote",
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q99",
    "Language" = "Q2",
    "Religion" = "Q98A",
    "Ethnicity" = "Q81",
    "Weight" = "Combinwt_new_hh",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(DATEINTR, format = "%Y")) %>%
      mutate(Ethnicity = fct_recode(Ethnicity, "Portuguese" = "3")) %>%
      mutate(Religion = fct_recode(Religion, 
        "Christian only" = "Christian only (i.e., respondents says only “Christian”, without identifying a specific sub-group)",
        "Muslim only" = "Muslim only (i.e., respondents says only “Muslim”, without identifying a specific sub-group)",
        "Sunni only" = "Sunni only (i.e., respondents says only “Sunni” or “Sunni Muslim”, without identifying a specific sub-group)"
      ))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Refused", "Not asked in this country", "Not Applicable", "Don’t know"),
    "Other" = c("Other")
  ),
  Language = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other"),
    "No Religion" = c("None", "Agnostic (Do not know if there is a God)", "Atheist (Do not believe in a God)")
  ),
  Ethnicity = list(
    "Other" = c("Other", "Relacionado com a origem regional (badio/sampadjudo)", "Relacionado com a Idade",
                "Relacionado com o Género", "Relacionado com a Ocupação", "Relacionado com a Religião",
                "Relacionado com a Classe", "Relacionado com a Raça", 
                "(National identity) only, or “doesn’t think of self in those terms”"),
    "Missing" = c("Missing", "Not asked in the country", "Not asked in this country", "Don’t know")
  )
)
