data.spec <- list(
  file.name = "Divided/datasets/pew/2012/Pew Research Global Attitudes Project Spring 2012 Dataset for web.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "Do you consider yourself as belonging to a particular religion? (If yes) Which one? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic/nationality/tribal group do you belong to? [Question wording varies by country]"
  ),
  skip.countries = list(
    no_party = c("China", "Egypt")
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
    # Coalesce necessary vars
    data <- coalese.vars(data, c('RELIG', str_subset(names(data), regex("^Q61", ignore_case = T))), "relig")
    
    party.vars <- c("PARTY", str_subset(names(data), regex("^Q164", ignore_case = T))) %>%
      discard(~ .x %in% c('Q164CHI')) # China removed since they don't ask about party choice
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- c('RACE1M.1', str_subset(names(data), "^Q159"))
    data <- coalese.vars(data, eth.vars, "eth")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2012)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_squish(levels(data[[var]]))
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "Don't know","None/No party (Volunteered)", "None", "None (Volunteered)", "None / No party (Volunteered)",
                  "None/No party (Volunteered)", "Would not vote (Volunteered)", "Would not vote (Volunteered)", "Blank /null vote (Volunteered)",
                  "No preference (Volunteered)", "Don't know", "None / No party", "Will not vote (Volunteered)", "Nothing (Volunteered)",
                  "None\\No Party (Volunteered)", "Blank (Volunteered)", "Don\u2019t know", "Don\u2019t know/haven't decided yet", "Undecided"),
    "Other" = c("Independent", "Other party (Volunteered)", "Other", "Other (Volunteered)", "Independent candidate",
                "Independent candidate/Azaad Umeedwaar", "Other party")
  ),
  Religion = list(
    "Missing" = c("Don't know", "Nothing in particular", "Refused", "Belongs to a religion, but does not want to say which one (Volunteered)",
                  "No religion (Volunteered)", "None"),
    "Other" = c("Something else", "Agnostic (not sure if there is a God)", "Atheist (do not believe in God)", "Other religion (Volunteered)",
                "No religion/not a believer/Atheist/Agnostic (Volunteered)", "No believer / atheist / agnostic (Volunteered)",
                "No religion/not a believer/atheist/agnostic (Volunteered)", "Other (Volunteered)",
                "Other", "No religion", "Without religion/Atheist", "Atheist / not believer",
                "A believer of no religion in particular", "No religion/not a believer/atheist/agnostic")
  ),
  Ethnicity = list(
    "Missing" = c("Refused", "Don't know", "Refused (e.g., non-race answers like American, Human, purple)", "Don\u2019t know"),
    "Other" = c("Other", "Others", "Other (Volunteered)", "Other European country (Germany... etc.)", "Any other country",
                "Other countries")
  )
)
