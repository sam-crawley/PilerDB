data.spec <- list(
  file.name = "Pew Research Global Attitudes Spring 2014 Dataset for Web.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "What is your current religion, if any?",
    "Ethnicity" = "Which ethnic/nationality/tribal group do you belong to? [Question wording varies by country]",
    "Language" = "What languages do you usually speak at home?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Peru" = "PresCand"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = "lang",
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "weight",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  manual.exclusions = c('United Kingdom'),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    data <- coalese.vars(data, c('RELIG', str_subset(names(data), regex("^QREL", ignore_case = T))), "relig")
    
    party.vars <- c("PARTY", str_subset(names(data), regex("^Q158", ignore_case = T))) %>%
      discard(~ .x %in% c('Q158CHI', 'Q158MEX')) # China / Mexico removed since they don't ask about party choice
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- str_subset(names(data), "^Q152")
    data <- coalese.vars(data, eth.vars, "eth")
    
    data <- coalese.vars(data, c("UKR11"), "lang")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2014)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "Don't know", "None/No party", "None/No party (Volunteered)", "None", "None (Volunteered)",
                  "None/No party  (Volunteered)", "None\\No party (Volunteered)", "None/No party", "None / No party (Volunteered)",
                  "None/No party (Volunteered)", "Would not vote (Volunteered)", "Would not vote (Volunteered)",
                  "Don\u2019t know/haven\u2019t decided yet (Volunteered)", "Don t know", "No preference (Volunteered)", "Blank", "Missing",
                  "Don\u2019t know", "None / No party", "Dont know", "Would not vote  (Volunteered)", "Blank /null vote (Volunteered)",
                  "Undecided", "Nothing (Volunteered)", "None  (Volunteered)"),
    "Other" = c("Independent", "Other party (Volunteered)", "Other", "Other   (Volunteered)", "Other (Volunteered)",
                "Other  (Volunteered)", "Other (SPECIFY)", "Other party (SPECIFY)", "A party with credible candidate (Volunteered)",
                "Any party (Volunteered)", "Independent candidate/Azaad Umeedwaar", "Independents")
  ),
  Language = list(
    "Missing" = c("Missing"),
    "Other" = c("Other")
  ),
  Religion = list(
    "Missing" = c("Don\u2019t know", "Don't know", "Refused", "Don't Know/Refused", "Dont know"),
    "Other" = c("Something else", "Something else (SPECIFY)", "Something else (Volunteered)"),
    "No Relgion" = c("Agnostic (not sure if there is a God)", "Atheist (do not believe in God)", "Agnostic", "Atheist", "Nothing in particular", 
                     "Nothing in particular, or", "Nothing in particular or")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Refused", "Don't know", "Don\u2019t know"),
    "Other" = c("Other", "Others")
  )
)
