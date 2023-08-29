data.spec <- list(
  file.name = "ESS5e03_4.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party feel closer to",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  party.question.type = "Closest",
  file.encoding = "latin1",
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
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "^prtcl"), "prtcl")
    
    rl.cols <- str_subset(names(data), "^rlgdn") %>%
      discard(~.x == "rlgdnme")
    
    data <- coalese.vars(data, rl.cols, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2010) %>%
      
      # Incorporate response from rlgblg to indicate if they have no religion
      mutate(rlgblg = haven::as_factor(rlgblg)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, rlgblg == "No", "No Religion"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know","No answer", "No one"),
    "Other" = c("Outro", "Other", "Independent", "Andet - other", "Other party", "Annat parti", "Andere Partei", "Other (nir)",
                "Invalid", "other")
  ),
  Language = list(
    "Missing" = c("777", "888", "999", "ZXX")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable")
  )
)
