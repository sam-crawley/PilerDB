data.spec <- list(
  file.name = "R9.Merge_39ctry.20Nov23.final.release_Updated.25Oct24 2 2.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "If [presidential elections] were held tomorrow, which [candidate's party] would you vote for?",
    "Language" = "What is the primary language you speak in your home now?",
    "Religion" = "What is your religion, if any?",
    "Ethnicity" = "What is your ethnic community or cultural group?"
  ),
  party.question.type = "PresPartyVote",
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q96",
    "Language" = "Q2",
    "Religion" = "Q95",
    "Ethnicity" = "Q84A",
    "Weight" = "Combinwt_new_hh",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(DATEINTR, format = "%Y")) %>%
      mutate(Religion = fct_recode(Religion, 
           "Christian only" = "Christian only (i.e., respondents says only \u201CChristian\u201D, without identifying a specific sub-group)",
           "Muslim only" = "Muslim only (i.e., respondents says only \u201CMuslim\u201D, without identifying a specific sub-group)",
           "Sunni only" = "Sunni only (i.e., respondents says only \u201CSunni\u201D or \u201CSunni Muslim\u201D, without identifying a specific sub-group)"
      ))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Don\u2019t know", "Not asked in the country", "Would not vote", "Refused to answer"),
    "Other" = c("Other")
  ),
  Language = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused", "Not asked in the country"),
    "Other" = c("Other"),
    "No Religion" = c("None", "Agnostic (Do not know if there is a God)", "Atheist (Do not believe in a God)")
  ),
  Ethnicity = list(
    "Other" = c("Other", "(National identity) only, or \u201Cdoesn\u2019t think of self in those terms\u201D", 
                "Related to regional origin (badio/sampadjudo)", "Related to age",
                "Related to gender",
                "Related to occupation",
                "Related to religion",
                "Related to class",
                "Related to race",
                "Related to political affiliation", 
                "Related to a regional origin (Foros, Angulares, Cape Verdeans, Principienses)",
                "Related to party affiliation"),
    "Missing" = c("Missing", "Not asked in the country", "Don\u2019t know", "Refused to answer")
  )
)
