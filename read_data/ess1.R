data.spec <- list(
  file.name = "Divided/datasets/ess/Round 1/ESS1e06_6.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party feel closer to",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  file.encoding = "latin1",
  skip.countries = list(),
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
    data %>% mutate(Year = 2002) %>%
      
      # Incorporate response from rlgblg to indicate if they have no religion
      mutate(rlgblg = haven::as_factor(rlgblg)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, rlgblg == "No", "No Religion"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know", "No answer", "Don t know"),
    "Other" = c("Other",  "Others", "Autres", "Other parties", "Other party", "OTHER", "Autre", "Andere", "Andere Partei",
                "Other (nir)")
  ),
  Language = list(
    "Missing" = c("777", "888", "999")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable")
  )
)
