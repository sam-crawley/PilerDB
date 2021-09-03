data.spec <- list(
  file.name = "Divided/data/pew/2019/Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav",
  file.type = 'sav',
  skip.countries = list(
    no_party = c('Lebanon', 'South Korea'),
    no_group = c("Tunisia", "Turkey"),
    low_n = c("Philippines")
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
    "Missing" = c("Do not feel close to any party", "No sabe (Dont know)", "No responde (Refused)", "Don’t know", "Refused", "Nao sabe (Don’t know)", 
                  "Outro (Other)", "Recusa (Refused)", "Dont know", "Menolak (Refused)", "No contestó (Refused)", "No sabe (Don’t know)",
                   "(VOL) Don't know", "(VOL) Refused", "Tidak tahu (Don’t know)"),
    "Other" = c("Otro (Other)", "Other", "Independent", "Lainnya (Other)", "(VOL) Other party", "(VOL) No preference")
  ),
  Language = list(
    "Missing" = c("Don’t know", "Refused")
  ),
  Religion = list(
    "Missing" = c("Refused", "Don’t know", "Dont know", "(VOL) Refused", "Something else (SPECIFY), or", "(VOL)\xc2\xa0Don't know",
                  "Something else or"),
    "Other" = c("Atheist (do not believe in God)", "Agnostic (not sure if there is a God)", "Something else, or", "Nothing in particular",
                "Agnostic (I dont really know whether there is a god, or whether there are any gods)", "Atheist (I do not believe in any gods or God)")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Don’t know", "Refused", "Dont know"),
    "Other" = c("Other", "Other (UNSPECIFIED)")
  )
)
