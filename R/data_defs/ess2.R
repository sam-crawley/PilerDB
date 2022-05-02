data.spec <- list(
  file.name = "ESS2e03_6.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party feel closer to",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  file.encoding = "latin1",
  country.format = 'iso2c',
  field.def = c(
    "Party" = "prtcl",
    "Language" = "lnghoma",
    "Religion" = "rlgdnm",
    "Ethnicity" = NA,
    "Weight" = "pspwght",
    "Country" = "cntry",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "^prtcl"), "prtcl")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2004) %>%
      
      # Incorporate response from rlgblg to indicate if they have no religion
      mutate(rlgblg = haven::as_factor(rlgblg)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, rlgblg == "No", "No Religion"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know", "No answer"),
    "Other" = c("Other", "Others", "Autres", "Other (WRITE DOWN) ___", "Other parties", "Other-minor parties",
                "Other (nir)", "Autre", "Other-minor parties", "Andere")
  ),
  Language = list(
    "Missing" = c("888", "999")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable")
  )
)
