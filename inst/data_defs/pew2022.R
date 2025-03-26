data.spec <- list(
  file.name = "Pew Research Center Global Attitudes Spring 2022 Dataset.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most idenify with/feel closest to? [Question wording varies by country]",
    "Language" = "What language do you speak at home?",
    "Religion" = "What is your present religion, if any? / How would you define yourself religiously? [Question wording varies by country]"
  ),
  party.question.type = "Closest",
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = "d_language_belgium",
    "Religion" = "relig",
    "Ethnicity" = NA,
    "Weight" = "weight",
    "Country" = "country",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalece necessary vars
    rel.vars <- str_subset(names(data), regex("^D_RELIG_", ignore_case = T)) %>%
      discard(~.x %in% c("d_relig_israel2", "d_relig_israel_jewish", "d_relig_japan_2"))
    
    data <- coalese.vars(data, rel.vars, "relig")
    
    data <- coalese.vars(data, str_subset(names(data), 
      regex("^D_PTYID_", ignore_case = T) %>% discard(~.x == "d_ptyid_proximity_lean")), "prty")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2022)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\032")
    }
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Do not feel close to any party", "Don\u2019t know", "Refused", "Don't know", "Don't know/Not sure",
                  "Refused/Prefer not to say", "Dont know", "DK/Refused"),
    "Other" = c("Other", "Independent", "Other party", "Other (SPECIFY)", "Other party (please specify)")
  ),
  Language = list(
    "Missing" = c("Refused")
  ),
  Religion = list(
    "Missing" = c("Refused", "Don\u2019t know", "Don't know", "Dont know", "DK/Refused", "Refused/Prefer not to say",
                  "Don't know/Not sure"),
    "Other" = c("Something else, or", "Something else (SPECIFY)", 
                "Confucianism, Shintoism, animism or other traditional or folk religion",
                "Confucianism, Muism or animism or other traditional or folk religions"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)",  "Nothing in particular",
                      "Agnostic (I dont really know whether there is a god, or whether there are any gods)", "Atheist (I do not believe in any gods or God)",
                      "Agnostic (not sure if there is a god)", "Atheist (do not believe in god)", "Athiest (do not believe in God)")
  )
)
