data.spec <- list(
  file.name = "Divided/datasets/pew/2020/Pew Research Center Global Attitudes Summer 2020 Dataset - Public.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most idenify with/feel closest to? [Question wording varies by country]",
    "Language" = "What language do you speak at home?",
    "Religion" = "What is your present religion, if any? / How would you define yourself religiously? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic [or tribal] group do you belong to?"
  ),
  skip.countries = list(
    no_party = c('South Korea')
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "prty",
    "Language" = "D_LANGUAGE_BELGIUM",
    "Religion" = "relig",
    "Ethnicity" = "d_race_us_1",
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
    
    data
  },
  fixups = function(data) {
    data <- data %>% mutate(Year = 2020)
    
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\s*\\(DO NOT READ\\)")
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "\\032")
    }
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Do not feel close to any party", "Don’t know", "Refused", "Dont know", "(VOL) Don't know", "(VOL) Refused"),
    "Other" = c("Other", "Independent", "(VOL) Other party", "(VOL) No preference", "Other party")
  ),
  Language = list(
    "Missing" = c("Don’t know", "Refused")
  ),
  Religion = list(
    "Missing" = c("Refused", "Don’t know", "Dont know", "(VOL) Refused", "Something else (SPECIFY), or", "(VOL)\u00a0Don\'t know"),
    "Other" = c("Something else, or"),
    "No Religion" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)",  "Nothing in particular",
                "Agnostic (I dont really know whether there is a god, or whether there are any gods)", "Atheist (I do not believe in any gods or God)")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Some other race")
    
  )
)
