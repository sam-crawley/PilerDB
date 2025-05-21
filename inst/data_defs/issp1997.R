data.spec <- list(
  file.name = "ZA3090.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Denmark" = "PartyVote",
    "Italy" = "PartyVote",
    "Netherlands" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "D-W" = "Germany",
    "D-E" = "Germany",
    "gb" = "United Kingdom",
    "usa" = "United States",
    "h" = "Hungary",
    "i" = "Italy",
    "nl" = "Netherlands",
    "n" = "Norway",
    "s" = "Sweden",
    "cz" = "Czechia",
    "slo" = "Slovenia",
    "pl" = "Poland",
    "bg" = "Bulgaria",
    "rus" = "Russia",
    "nz" = "New Zealand",
    "cdn" = "Canada",
    "rp" = "Philippines",
    "IL-J" = "Israel",
    "IL_A" = "Israel",
    "j" = "Japan",
    "e" = "Spain",
    "f" = "France",
    "cy" = "Cyprus",
    "p" = "Portugal",
    "dk" = "Denmark",
    "ch" = "Switzerland",
    "bup" = "Bangladesh"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "weight"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("prty")), ~if_else(.x == 0, NA, .x)))
    
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "prty$"), "prty")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      mutate(Year = 1997)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No party preference", "None", "Dont know", "Refused",
                  "na", "No answer", "No answer, refused", "No vote", "none",
                  "Would not vote;No party preference", "refused", "Cant choose",
                  "Don't know", "no party", "No particular party", "did not voted",
                  "DON'T KNOW YET", "None of these", "refuse", "NO PARTY",
                  "Would not vote;No pa", "Would not vote;No party",
                  "Refused to answer", "Did not vote/Returne",
                  "Did not have the right age", "Did not decided",
                  "will not vote", "NA/Refused", "Non Party Individuals",
                  "NOT ELIGIBLE", "don't Know", "BLANC VOTE", "Blank Ballot"),
    "Other" = c("Other party", "Other answer", "Other Party", "other party",
                "Other party - Left", "OTHER PARTY", "Others", "Other party - Religious",
                "OPPOSITION PARTY", "Other party - Right", "OTHER, NO SPECIFIC")
  ),
  Religion = list(
    "Missing" = c("na", "NAV, NAP", "dk", "Refusal"),
    "Other" = c("Other non-christian", "No denom given", "Other n classified"),
    "No Religion" = c("None")
  )
)