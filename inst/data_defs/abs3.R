data.spec <- list(
  file.name = "ABS3 merge20210506.sav",
  file.type = 'sav',
  question.text = c(
    "Religion" = "What is your religion?"
  ),
  party.question.type = "PartyVote",
  country.party.question.type = c(
    "Taiwan" = "PresCand",
    "South Korea" = "PresCand",
    "Philippines" = "PresCand",
    "Mongolia" = "PresCand"
  ),  
  country.format = 'country.name',
  field.def = c(
    "Party" = "q33",
    "Religion" = "se6",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "country",
    "Year" = NA,
    "Weight" = "couweight"
  ),
  fixups = function(data) {
    data %>% mutate(Year = case_when(
      Country == "Taiwan" ~ 2010,
      Country == "Philippines" ~ 2010,
      Country == "Mongolia" ~ 2010,
      Country == "Thailand" ~ 2010,
      Country == "Malaysia" ~ 2011,
      Country == "Indonesia" ~ 2011,
      Country == "Japan" ~ 2011,
      Country == "South Korea" ~ 2011,
      Country == "Cambodia" ~ 2012,
      Country == "China" ~ 2011,
      Country == "Singapore" ~ 2010,
      Country == "Vietnam" ~ 2010,
      TRUE ~ 0
    ))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Missing", "Can't choose", "Decline to answer", "No vote", "Dont' know", "None", "Cast invalid vote"),
    "Other" = c("Other (Termasuk undi rosak)", "others", "Others(HK)", "Others", "independent")
  ),
  Religion = list(
    "Missing" = c("Missing", "Decline to answer", "Cannot choose"),
    "Other" = c("Other"),
    "No Religion" = c("None")
  )
)