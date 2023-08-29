data.spec <- list(
  file.name = "Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Religion" = "What is your present religion, if any? / How would you define yourself religiously? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic [or tribal] group do you belong to?"
  ),
  party.question.type = "Closest",
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = NA,
    "Religion" = "relig",
    "Ethnicity" = "eth",
    "Weight" = "weight",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalece necessary vars
    rel.vars <- str_subset(names(data), regex("^D_RELIG_", ignore_case = T)) %>% 
      discard(~.x %in% c("d_relig_us2", "d_relig_us_christian"))
    
    data <- coalese.vars(data, rel.vars, "relig")
    
    data <- coalese.vars(data, str_subset(names(data), regex("^D_PTYID_", ignore_case = T)), "prty")
    
    data <- coalese.vars(data, c(str_subset(names(data), "^d_ethnicity"), "d_race_us_1"), "eth")
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2018)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\032")
    }
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Do not feel close to any party", "No responde (Refused)", "Don\u2019t know", "Refused", "No contest", "No sabe (Don't know)",
                   "Recusa (Refused)", "Menolak (Refused)", "Tidak tahu (Don\u2019t know)", "No preference", "Don't know", "No sabe (Don't Know)",
                  "Nao sabe (Don't know)"),
    "Other" = c("Other", "Independent", "Other party", "Other (SPECIFY))", "Other (Specify)", "Otro (ESPECIFICAR) (Other (SPECIFY))",
                "Outro (ESPECIFICAR) (Other(SPECIFY))", "Other (SPECIFY)", "Lainnya (Specify) (Other (SPECIFY))")
  ),
  Language = list(
  ),
  Religion = list(
    "Missing" = c("Refused", "Don\u2019t know", "Something else (SPECIFY), or", "Don't know"),
    "Other" = c("Something else (SPECIFY)"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)", "Nothing in particular",
                "Atheist (I do not believe in any gods or God)", "Agnostic (I don't really know whether there is a god, or whether there are any gods)")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Don't know", "Some other race (Specify____ IF NEEDED: What race or races is that?)",
                    "Don't know", "Refused (e.g., non-race answers like American, human, purple)", "Don\u2019t know", "Refused"),
    "Other" = c("Other (SPECIFY)", "Other (UNSPECIFIED)")
  )
)
