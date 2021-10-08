data.spec <- list(
  file.name = "Divided/datasets/pew/2011/Pew Global Attitudes Spring 2011 Dataset WEB.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "What is your current/present religion, if any?? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic [or tribal] group do you belong to?"
  ),
  skip.countries = list(
    no_party = c("China", "France", "Germany", "Mexico", "Spain", "United Kingdom")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = NA,
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "WEIGHT",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalece necessary vars
    data <- coalese.vars(data, c('RELIG', str_subset(names(data), regex("^Q34", ignore_case = T))), "relig")
    
    party.vars <- c("PARTY", str_subset(names(data), regex("^Q129", ignore_case = T))) %>%
      discard(~ .x %in% c('Q129CHI', 'Q129MEX')) # China / Mexico removed since they don't ask about party choice
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- c('RACE1', str_subset(names(data), "^Q124"))
    data <- coalese.vars(data, eth.vars, "eth")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2011)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_squish(levels(data[[var]]))
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "Don't know", "None", "Don't know", "None / No party", "Undecided", "Don\u2019t know", "None/No party", "None of the parties"),
    "Other" = c("Independent", "Independent candidate", "Other", "Other (SPECIFY)",
                "Independent candidate/Azaad Umeedwaar", "Other party")
  ),
  Religion = list(
    "Missing" = c("Don't know", "Nothing in particular", "Refused", "None", "Don\u2019t know"),
    "Other" = c("Atheist (do not believe in God)", "Other", "No religion", "No religion/not a believer/atheist/agnostic", "Other religion", "Other (SPECIFY)",
                "Any other", "Agnostic (not sure if there is a God)")
  ),
  Ethnicity = list(
    "Missing" = c("Refused", "Don\u2019t know"),
    "Other" = c("Other", "Or some other race", "Other (SPECIFY)")
  )
)
