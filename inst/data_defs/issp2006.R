data.spec <- list(
  file.name = "ZA4700_v2-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom",
    "DE-E" = "Germany",
    "DE-W" = "Germany",
    "IL (A)" = "Israel",
    "IL (J)" = "Israel"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "RELIG",
    "Language" = NA,
    "Ethnicity" = "ETHNIC",
    "Country" = "c_alphan",
    "Year" = "Year",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY")), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(Year = 2006) %>%
      mutate(prty = if_else(c_alphan == "US", "Missing", prty))
    
    data
  },
  fixups = function(data) {
    data %>%
      mutate(across(c(Party, Religion, Ethnicity), ~str_replace(.x, "^\\d+\\.\\s+", "")))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Uncertain", "Against all/ threw out/ damaged voting paper",
                  "No party preference", "No answer", "Don't know", "Did not vote",
                  "Would not vote", "Don't support any party", "Refused",
                  "Would not vote, no party preference", "No party affiliation or support",
                  "None, no party preference", "None, no preference",
                  "Did not vote,not eligible", "None", "No party, no preference",
                  "Will not vote", "No party", "No party affiliation",
                  "None of these", "Not eligible", "Would not vote,no preference",
                  "Vote blank", "Don't know, can't recall", "Did not vote/not eligible",
                  "Wouldn't vote", "Not interested so much in politics", "No answer, refused",
                  "Voted blank", "Would not vote; no party preference"),
    "Other" = c("Other party", "Other answer", "Other Party", "Other, center, center-right",
                "Other, left", "Independent", "Independents", "Other (specify)",
                "Other, no clear specification", "Other Parties", 
                "Other Party, Healthcare,Feministic Init,June List", 
                "Other party,specify", "Other Party:UKIP,BNP/NF, Scottish Socialist, Respect, Socialist")
  ),
  Religion = list(
    "Missing" = c("No answer;not available:GB-NIR", "Don't know", "Refused"),
    "Other" = c("Other Religions", "Other non-Christian Religions"),
    "No Religion" = c("No religion,not believe in God,Agnostic")
  ),
  Ethnicity = list(
    "Missing" = c("Not available; NAP", "No answer, don't know"),
    "Other" = c("Other,mixed origin", "SE:both parents Swedish citizen",
                "SE:One parent non-Swedish,one Swedish",
                "SE:Both parents non-Swedish")
  )
)