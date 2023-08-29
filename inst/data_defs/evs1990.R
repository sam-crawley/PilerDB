data.spec <- list(
  file.name = "ZA4460_v3-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "If there was a general election tomorrow, which party would you vote for?",
    "Religion" = "Do you belong to a religious denomination? [If yes] Which one?"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom"
  ),
  field.def = c(
    "Party" = "q674a",
    "Language" = NA,
    "Religion" = "q333b",
    "Ethnicity" = NA,
    "Weight" = "weight_g",
    "Country" = "c_abrv",
    "Year" = "year"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\s*[\\w-]+:\\s*")
    }
    
    # Incorporate response from Q332A to indicate if they have no religion
    data <- data %>% mutate(q332a = haven::as_factor(q332a)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, q332a == "No", "No Religion"))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "I would not vote", "I would cast a blank ballot", "No right to vote", "None", "undocumented", "I would not go to election",
                  "unknown", "no vote"),
    "Other" = c("Other", "other party", "other", "other party/movement", "Other party", "Other/independent", "Other parties", "Well governing, just, wise party",
                "Depends on candidates, on programs, will vote for people", "Non-party people", "None, will not vote for any party, not for any other",
                "Other answers", "Independent", "Een andere partij", "andere partij")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("Other")
  )
)
