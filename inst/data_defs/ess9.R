data.spec <- list(
  file.name = "ESS9e03_1.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party feel closer to",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),
  party.question.type = "Closest",
  country.format = 'iso2c',
  field.def = c(
    "Party" = "prtcl",
    "Language" = "lnghom1",
    "Religion" = "rel",
    "Ethnicity" = NA,
    "Weight" = "pspwght",
    "Country" = "cntry",
    "Year" = NA
  ),
  manual.exclusions = c('Cyprus'),
  pre_fixups = function(data) {
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "^prtcl"), "prtcl")
    
    rl.cols <- c("rlgdnbat", "rlgdnbe",  "rlgdnach", "rlgdncy",  "rlgdnade", "rlgdnafi", "rlgdngb",  "rlgdnhu",  "rlgdnie",  "rlgdnais",
                 "rlgdnlt",  "rlgdnlv",  "rlgdme",   "rlgdnnl",  "rlgdnno",  "rlgdnapl", "rlgdnrs",  "rlgdnase", "rlgdnask", "rlgdnme")
    
    data <- coalese.vars(data, rl.cols, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2018) %>%
      
      # Incorporate response from rlgblg to indicate if they have no religion
      mutate(rlgblg = haven::as_factor(rlgblg)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, rlgblg == "No", "No Religion"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know", "No answer"),
    "Other" = c("Other (nir)", "Altro", "Independent", "Autre", "Annat parti", "Ostalo", "Other")
  ),
  Language = list(
    "Missing" = c("777", "888", "999", "ZXX")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable")
  )
)
