data.spec <- list(
  file.name = "datasets/evs/1999/ZA3811_v3-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "If there was a general election tomorrow, which party would you vote for?",
    "Religion" = "Do you belong to a religious denomination? [If yes] Which one?"
  ),
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom"
  ),
  field.def = c(
    "Party" = "v256",
    "Language" = NA,
    "Religion" = "v102",
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
    
    # Incorporate response from v101 to indicate if they have no religion
    data <- data %>% mutate(v101 = haven::as_factor(v101)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, v101 == "no", "No Religion"))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "I would not vote", "I would cast a blank ballot", "no party mentioned", "No right to vote", "None of these parties",
                  "Refuse to answer", "None"),
    "Other" = c("Other", "other party", "some other party", "any party", "others", "Otro -other-", "Annat -other-", "other", "other party/movement",
                "Other party", "Independent", "Een andere partij", "andere partij")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("other")
  )
)
