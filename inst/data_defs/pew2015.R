data.spec <- list(
  file.name = "Pew Research Global Attitudes Spring 2015 Dataset for Web FINAL.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Language" = "What is your home language? / Which language do you, yourself, usually speak at home?",
    "Religion" = "What is your current/present religion, if any? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic/racial/tribal group do you belong to? [Question wording varies by country]"
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
    "Weight" = "WEIGHT",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  manual.exclusions = c('United Kingdom'),
  pre_fixups = function(data) {
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), regex("^Q78", ignore_case = T)), "relig")
    
    party.vars <- c("Q177", str_subset(names(data), regex("^Q182", ignore_case = T))) %>%
      discard(~ .x %in% c('Q182CHI', 'Q182MEX')) # China / Mexico removed since they don't ask about party choice
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- str_subset(names(data), "^Q168") %>% discard(~.x %in% c("Q168CAN", "Q168BRSA"))
    data <- coalese.vars(data, eth.vars, "eth")
    
    data <- coalese.vars(data, c("Q168CAN", "Q168BRSA", "Q189"), "lang")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2015)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "Don\u2019t know", "Don't know", "None/No party", "None/No party (Volunteered)", "None", "None (Volunteered)",
                  "None/No party  (Volunteered)", "None\\No party (Volunteered)", "None/No party", "None / No party (Volunteered)",
                  "Don't Know", "Undecided", "None of the above/No party", "Nothing (Volunteered)", "No preference (Volunteered)",
                  "Not enrolled to vote", "Would not vote  (Volunteered)"),
    "Other" = c("Independent", "Other party (Volunteered)", "Other", "Other (Volunteered)", "Other (Specify) (Volunteered)",
                "Other (Volunteered)", "Others (Volunteered)", "Independents", "Independent candidate/Azaad Umeedwaar")
  ),
  Language = list(
    "Missing" = c("Refused", "Missing"),
    "Other" = c("Other (SPECIFY)", "Other")
  ),
  Religion = list(
    "Missing" = c("Don\u2019t know", "Don't know", "Refused"),
    "Other" = c("Something else", "Something else (SPECIFY)"),
    "No Religion" = c("Nothing in particular", "Nothing in particular, or", "Agnostic",  "Atheist", 
                      "Agnostic (not sure if there is a God)", "Atheist (do not believe in God)")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Refused", "Don't know", "Don\u2019t know"),
    "Other" = c()
  )
)
