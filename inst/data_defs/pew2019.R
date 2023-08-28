data.spec <- list(
  file.name = "Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Language" = "What language do you speak at home?",
    "Religion" = "What is your present religion, if any? / How would you define yourself religiously? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic [or tribal] group do you belong to?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = "lng",
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "weight",
    "Country" = "country",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalece necessary vars
    rel.vars <- str_subset(names(data), regex("^D_RELIG_", ignore_case = T)) %>% 
      discard(~.x %in% c("d_relig_us2", "d_relig_us_christian"))
    
    data <- coalese.vars(data, rel.vars, "relig")
    
    data <- coalese.vars(data, str_subset(names(data), regex("^D_PTYID_", ignore_case = T)), "prty")
    
    data <- coalese.vars(data, str_subset(names(data), "^D_ETHNICITY"), "eth")
    
    data <- coalese.vars(data, str_subset(names(data), "^LANGUAGE_HOME_"), "lng")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2019)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\032")
    }
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Do not feel close to any party", "No sabe (Dont know)", "No responde (Refused)", "Don\u2019t know", "Refused", "Nao sabe (Don\u2019t know)", 
                  "Outro (Other)", "Recusa (Refused)", "Dont know", "Menolak (Refused)", "No contest\u00F3 (Refused)", "No sabe (Don\u2019t know)",
                   "(VOL) Don't know", "(VOL) Refused", "Tidak tahu (Don\u2019t know)"),
    "Other" = c("Otro (Other)", "Other", "Independent", "Lainnya (Other)", "(VOL) Other party", "(VOL) No preference")
  ),
  Language = list(
    "Missing" = c("Don\u2019t know", "Refused")
  ),
  Religion = list(
    "Missing" = c("Refused", "Don\u2019t know", "Dont know", "(VOL) Refused", "Something else (SPECIFY), or", "(VOL)\u00a0Don\'t know",
                  "Something else or"),
    "Other" = c("Something else, or"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)",  "Nothing in particular",
                "Agnostic (I dont really know whether there is a god, or whether there are any gods)", "Atheist (I do not believe in any gods or God)")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Don\u2019t know", "Refused", "Dont know"),
    "Other" = c("Other", "Other (UNSPECIFIED)")
  )
)
