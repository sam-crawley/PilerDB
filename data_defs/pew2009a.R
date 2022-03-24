data.spec <- list(
  file.name = "datasets/pew/2009spring/Pew Gap 2009 Data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "What is your present religion, if any?",
    "Ethnicity" = "Which ethnic/nationality/tribal group do you belong to? [Question wording varies by country]"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = NA,
    "Religion" = "RELIG",
    "Ethnicity" = "eth",
    "Weight" = "WEIGHT",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    party.vars <- c("PARTY", str_subset(names(data), regex("^Q103", ignore_case = T))) %>%
      discard(~ .x %in% c('Q103MEX'))
    
    data <- coalese.vars(data, party.vars, "prty")
    
    eth.vars <- c('RACE', str_subset(names(data), "^Q98"))
    data <- coalese.vars(data, eth.vars, "eth")
    
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
    "Missing" = c("Refused", "Don't know", "None/No party (Volunteered)", "Don\u0092t know", "Don t know", "Dont know", "None / no party (Volunteered)",
                  "None \\ no party (Volunteered)", "None / No party (Volunteered)", "None / No party(Volunteered)", "No preference [Volunteered]",
                  "No one (Volunteered)", "Nothing (Volunteered)"),
    "Other" = c("Independent",  "Other (Volunteered)", "Other party (Volunteered)", "Independent candidates", "Independents", "Independent candidate")
  ),
  Religion = list(
    "Missing" = c("Don't know", "Refused"),
    "Other" = c("Something else (SPECIFY)"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)", "Nothing in particular")
  ),
  Ethnicity = list(
    "Missing" = c("Refused", "Don't know", "Don\u0092t know"),
    "Other" = c("Other", "Others", "From another country", "Other or mixed race", "other", "Other (Volunteered)")
  )
)
