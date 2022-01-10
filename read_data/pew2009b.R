data.spec <- list(
  file.name = "Divided/datasets/pew/2009fall/Pew Global Attitudes Fall 2009 Dataset.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "Do you consider yourself as belonging to a particular religion? (If yes) Which one? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic/nationality/tribal group do you belong to? [Question wording varies by country]",
    "Language" = "What language do you speak at home?"
  ),
  skip.countries = list(),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = "lang",
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "WEIGHT",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    data <- coalese.vars(data, c('RELIG', str_subset(names(data), regex("^Q67", ignore_case = T))), "relig")
    
    party.vars <- c("PARTY", str_subset(names(data), regex("^Q68", ignore_case = T)))
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- c('RACE', str_subset(names(data), "^Q63"))
    data <- coalese.vars(data, eth.vars, "eth")
    
    data <- coalese.vars(data, str_subset(names(data), "^Q52"), "lang")    
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2009)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_squish(levels(data[[var]]))
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "Don't know", "None", "Does not feel any party close to him/herself", "None/No party (Volunteered)",
                  "None of parties (Volunteered)", "None of the parties (Volunteered)", "None\\No party (Volunteered)",
                  "No preference (Volunteered)", "Don\u0092t know"),
    "Other" = c("Independent",  "Other", "other response (not a party, not an organisation)", "Other (Volunteered)", "Other party (Volunteered)",
                "Other party, organisation", "Other political party", "No such party")
  ),
  Language = list(
    "Missing" = c("Don\u0092t know", "Don't know", "Refused"),
    "Other" = c("Other", "other")
  ),
  Religion = list(
    "Missing" = c("Don't know", "Nothing in particular", "Refused", "Don't know/Refused"),
    "Other" = c("Other", "Other religion", "Other, not directly classifiable", "Other not catholic", 
                "Yes, I profess another faith", "Other (Volunteered)", "Something else"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)", "None",
                      "Does not belong to a religion", "No religion", "No, I do not profess any religion", "I do not belong to any religion", 
                      "Do not belong to a religion", "None (Volunteered)", "No particular religion", "I am non-affiliated", "Non-believer")
  ),
  Ethnicity = list(
    "Missing" = c("Refused", "Don't know", "Don\u0092t know"),
    "Other" = c("Other", "Others", "From another country", "Other or mixed race", "Other nationality", "Mixed race")
  )
)
