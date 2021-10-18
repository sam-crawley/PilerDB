data.spec <- list(
  file.name = "Divided/datasets/pew/2016/Pew Research Center Spring 2016 Global Attitudes Dataset WEB FINAL.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Language" = "What is your home language? / Which language do you, yourself, usually speak at home?",
    "Religion" = "What is your present religion, if any? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic group do you belong to?"
  ),
  skip.countries = list(
    no_party = c("China")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = "lang",
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "weight",
    "Country" = "country",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), regex("^Q109", ignore_case = T)), "relig")
    
    party.vars <- c("q126us", str_subset(names(data), regex("^Q131", ignore_case = T))) %>%
      discard(~ .x %in% c('Q131CHI')) # China removed since they don't ask about party choice
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- str_subset(names(data), "^Q119") %>% discard(~.x %in% c("Q119CAN", "Q119RSAb"))
    data <- coalese.vars(data, eth.vars, "eth")
    
    data <- coalese.vars(data, c("Q119CAN", "Q119RSAb"), "lang")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2016)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "No preference", "Don’t know", "Don't know", "None/No party", "None/No party (Volunteered)", "None", "None (Volunteered)",
                  "Would not vote (Volunteered)", "None/ No party"),
    "Other" = c("Independent", "Other party", "Other (Specify)", "Other party (Volunteered)", "Other", "Other (Volunteered)", "Other (SPECIFY)")
  ),
  Language = list(
    "Missing" = c("Refused", "Missing"),
    "Other" = c("Other (SPECIFY)", "Other (Specify)")
  ),
  Religion = list(
    "Missing" = c("Don’t know", "Don't know", "Don?t know", "Dont know", "Nothing in particular", "Refused", "Nothing in particular, or"),
    "Other" = c("Something else", "Agnostic", "Something else (SPECIFY)", "Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("Missing"),
    "Other" = c("Other (SPECIFY)", "Other (UNSPECIFIED)")
  )
)
