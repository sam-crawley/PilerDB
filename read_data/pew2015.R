data.spec <- list(
  file.name = "Divided/datasets/pew/2015/Pew Research Global Attitudes Spring 2015 Dataset for Web FINAL.sav",
  file.type = 'sav',
  question.text = c(
    "Party" = "Which party do you most identify with/feel closest to? [Question wording varies by country]",
    "Language" = "What is your home language? / Which language do you, yourself, usually speak at home?  (If you speak more than one language, which one do you speak most often?)",
    "Religion" = "What is your current/present religion, if any?? [Question wording varies by country]",
    "Ethnicity" = "Which ethnic [or tribal] group do you belong to?"
  ),
  skip.countries = list(
    no_party = c("Ethiopia", "Vietnam", "China", "Palestinian Territories", "Mexico"),
    no_group = c("Turkey"),
    low_n = c("Chile", "Philippines")
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
  pre_fixups = function(data) {
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), regex("^Q78", ignore_case = T)), "relig")
    
    data <- coalese.vars(data, c("Q177", str_subset(names(data), regex("^Q182", ignore_case = T))), "prty")
    
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
    "Missing" = c("Refused", "Don’t know", "Don't know", "None/No party", "None/No party (Volunteered)", "None", "None (Volunteered)",
                  "None/No party  (Volunteered)", "None\\No party (Volunteered)", "None/No party", "None / No party (Volunteered)"),
    "Other" = c("Independent", "Other party (Volunteered)", "Other", "Other (Volunteered)", "Other (Specify) (Volunteered)",
                "Other (Volunteered)")
  ),
  Language = list(
    "Missing" = c("Refused", "Missing"),
    "Other" = c("Other (SPECIFY)", "Other")
  ),
  Religion = list(
    "Missing" = c("Don’t know", "Don't know", "Nothing in particular", "Refused", "Nothing in particular, or"),
    "Other" = c("Something else", "Agnostic", "Something else (SPECIFY)", "Atheist")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Refused"),
    "Other" = c()
  )
)
