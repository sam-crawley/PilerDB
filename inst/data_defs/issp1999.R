data.spec <- list(
  file.name = "ZA3430.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  country.custom = c(
    "NIRL North Ireland" = "United Kingdom"
  ),
  field.def = c(
    "Party" = "x_prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "weight"
  ),
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Religion = if_else(Religion == "Prot (n else class), reformated", "Protestant (nec)", Religion)) %>%
      mutate(Year = 1999)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote", "None", 
                   "No preference", "Dont know", "Refused",
                   "Would not vote; No party preference", "na",
                   "No party proximity", "No party", "Would not vote; no party preference",
                   "Did not vote", "Dont Know", "Cant Choose", "No answer",
                   "Would not vote; not eligible", "No answer, refused",
                   "No vote", "NAP, NAV, NA, DK", "Would not vote; No party prefrence",
                   "No party, no preference", "Na/ref", "No party preference; wouldnt vote",
                   "Dk", "Na", "dk"),
    "Other" = c("Other party", "Other answer", "Other Party", "Non party individuals",
                "Other left party", "Other right party", "Other religious party")
  ),
  Religion = list(
    "Missing" = c(),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None", "No religion")
  )
)