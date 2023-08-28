data.spec <- list(
  file.name = "Pew Research Global Attitudes Spring 2017 Dataset WEB FINAL.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "What is your present religion, if any? / How would you define yourself religiously? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic [or tribal] group do you belong to?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = NA,
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "weight",
    "Country" = "Country",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalece necessary vars
    rel.vars <- str_subset(names(data), regex("^D_RELIG_", ignore_case = T)) %>% 
      discard(~.x %in% c("d_relig_us2", "d_relig_us_christian"))
    
    data <- coalese.vars(data, rel.vars, "relig")
    
    party.vars <- str_subset(names(data), regex("^D_PTYID_", ignore_case = T)) %>%
      discard(~ .x %in% c('d_ptyid_vietnam')) # Vietnam removed since they don't ask about party choice
    
    data <- coalese.vars(data, party.vars, "prty")
    
    data <- coalese.vars(data, c(str_subset(names(data), "^d_ethnicity"), "d_race_us"), "eth")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2017)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      #levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\032")
    }    
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Do not feel close to any party", "No responde (Refused)", "Refused", "No preference", "Don\u2019t know", "Don't know",
                  "Recusa (Refused)", "Menolak (Refused)", "Tidak tahu (Don\u2019t know)", "No sabe (Don\u2019t know)", "No contest\u00F3 (Refused)",
                  "Nao sabe (Don\u2019t know)", "Ne sait pas (Don\u2019t know)", "Refus (Refused)", "Kataa Kujibu (refused to respond)",
                  "Hajui (Don\u2019t know)", "Don\u2019t Know"),
    "Other" = c("Independent", "Other (SPECIFY))", "Other (Specify)", "Otro (ESPECIFICAR) (Other (SPECIFY))", "Other party",
                "Outro (ESPECIFICAR) (Other(SPECIFY))", "Other (SPECIFY)", "Lainnya (Specify) (Other (SPECIFY))",
                "Autres (A PRECISER) (Other (SPECIFY))", "Vingine (SPECIFY) (Other (SPECIFY))")
  ),
  Language = list(
  ),
  Religion = list(
    "Missing" = c("Something else (SPECIFY), or", "Don\u2019t know", "Don't know", "Refused"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)", "Nothing in particular",
                      "Atheist (I do not believe in any gods or God)", "Agnostic (I don't really know whether there is a god, or whether there are any gods)")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Some other race (Specify____ IF NEEDED: What race or races is that?)", "Refused (e.g., non-race answers like American, human, purple)",
                  "Don't know", "Don\u2019t know", "Refused"),
    "Other" = c("Other (SPECIFY)", "Other (UNSPECIFIED)")
  )
)
