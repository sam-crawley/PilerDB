data.spec <- list(
  file.name = "ZA7500_v5-0-0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which (political) party appeals to you most?",
    "Religion" = "Do you belong to a religious denomination? [If yes] Which one?"
  ),
  party.question.type = "Closest",
  country.format = 'iso2c',
  field.def = c(
    "Party" = "v174_cs",
    "Language" = NA,
    "Religion" = "v52_cs",
    "Ethnicity" = NA,
    "Weight" = "dweight",
    "Country" = "c_abrv",
    "Year" = "year"
  ),
  manual.exclusions = c('United Kingdom'),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\s*\\w+:\\s*")
    }
    
    # Make missing weights == 1
    data <- data %>% 
      mutate(Weight = as.numeric(Weight)) %>%
      mutate(Weight = if_else(Weight == -4, 1, Weight))
    
    # Incorporate response from v51 to indicate if they have no religion
    data <- data %>% mutate(v51 = haven::as_factor(v51)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, v51 == "no", "No Religion"))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("not applicable", "no answer", "dont know", "Blank vote"),
    "Other" = c("No (other) party appeals to me (spontaneous)", "No [no other] party appeals to me (spontaneous)", "No [,no other] party appeals to me",
                "No [,no other] party appeals to me (spontaneous)", "No [, no other] party appeals to me (spontaneous)", "Other", "Other, please specify (WRITE IN)",
                "No [no other] party appeals to me", "No (no other) party appeals to me", "No [, no other] party appeals to me",
                "Other, please specify (WRITE IN):", "Other party (WRITE IN)", "No party appeals to me (spontaneous)")
  ),
  Religion = list(
    "Missing" = c("not applicable", "no answer", "dont know"),
    "Other" = c("Other, please specify (Write in)", "Other than Christian", "Other", 
                "Other religious denomination, please specify (Write in)", "Other, which?",
                "Other Non Christian", "Other non-Christian (WRITE IN)", "Small religious communities",
                "Other Islamic Community of Faith", "Other Christian", 
                "Other Christian Religions (Jeovah's Withnesses, Mormons, etc.)",
                "Other, Non-Christian Religion", "Other Evangelical (e.g. Baptists, Methodists)",
                "Other Protestants", "Other Christian (specify)", "Other non-Christian (specify)",
                "Other protestant (which one?)", "Other Protestant (WRITE IN)",
                "Other Christian (WRITE IN)")
  )
)
