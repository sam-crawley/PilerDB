data.spec <- list(
  file.name = "20230504_W5_merge_15.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which parties or candidates did you vote for?",
    "Religion" = "What is your religion?",
    "Ethnicity" = "What's your racial (ethnic) background?"
  ),
  party.question.type = "PartyVote",
  country.party.question.type = c(
    "Taiwan" = "PresPartyVote",
    "South Korea" = "PresCand",
    "Philippines" = "PresCand",
    "Mongolia" = "PresPartyVote",
    "Indonesia" = "PresCand"
  ),    
  country.format = 'country.name',
  field.def = c(
    "Party" = "q34",
    "Religion" = "SE6",
    "Language" = NA,
    "Ethnicity" = "Se11a",
    "Country" = "country",
    "Year" = "Year",
    "Weight" = "w"
  )
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Invalid vote", "Missing", "Decline to answer", "Can't remember",
                  "Unmarked ballot paper", "No Party", "nota"),
    "Other" = c("Independent", "Other", "Parties in the respective region", 
                "Other Pro-Beijing camp", "Other Pan-democracy camp", "Others", "Etc.", "Individual canidate",
                "Some other party")
  ),
  Religion = list(
    "Missing" = c("Missing", "Decline to answer", "Not applicable"),
    "Other" = c("Others", "Other Asian religions", "Other Spiritual Beliefs nfd"),
    "No Religion" = c("None")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer"),
    "Other" = c("Others", "Other", "Other (please specify)", "other", "Others bumiputera")
  )
)