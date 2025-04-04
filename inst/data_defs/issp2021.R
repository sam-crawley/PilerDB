data.spec <- list(
  file.name = "ZA8000_v2-0-0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If elections where this Sunday, which party would you vote for?",
    "Religion" = "What is your religion?",
    "Ethnicity" = "What ethnicity or race you identify best with?"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = "ethn",
    "Country" = "c_alphan",
    "Year" = "DATEYR",
    "Weight" = "WEIGHT_COM"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY"), ends_with("RELIG"), ends_with("ETHN1")), ~if_else(.x == -2, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    data <- coalese.vars(data, str_subset(names(data), "RELIG$"), "relig")
    data <- coalese.vars(data, str_subset(names(data), "ETHN1$"), "ethn")
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("-4. NAP (Code 2, -4, -7 in VOTE_LE)", "-9. No answer", "-8. Don't know, don't remember", "-7. Refused",
                  "-4. NAP (Code 2, -4, in VOTE_LE)", "94. Mixed vote (Candidates of several lists)",
                  "-8. Don't know, don't remember", "-1. Not available", "-8. Can't say", "-8. Don't remember",
                  "-8. Don't know/ Can't remember", "-9. No answer + Don't remember", "96. Invalid ballot"),
    "Other" = c("95. Other party", "90. Independent candidate", "18. Other", "4. Independent", "4. Other candidate",
                "7. Other")
  ),
  Religion = list(
    "Missing" = c("-9. No answer", "-7. Refused", "-9. No answer, Don't know", "-8. Can't choose"),
    "Other" = c("11. Yes, consider to belong to a religion but: No answer as to which religion",
                "5. Other", "7. Other non-christians", "13. Other religions", "12. Other Asian religions",
                "970. Other non-Christian religions", "9. Other Asian religions", "10. Other religions",
                "5. Other Non-Christians", "960. Other religions", "10. Other religious denomination",
                "10. Yes, consider to belong: Other religion", "12. Other religions"),
    "No Religion" = c("0. No religion", "0. No, do not belong to any religion or denomination",
                      "0. No religion/ None", "0. No, do not consider to belong to a religion")
  )
)