data.spec <- list(
  file.name = "ESS10.dta",
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
    
    # Make sure the 'generic' rlgdnm is last (so country-specific vars get used if they exist)
    relvars <- str_subset(names(data), "^rlgdn") %>% discard(~.x == "rlgdnm") %>% append("rlgdnm")
    
    data <- coalese.vars(data, relvars, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = format.Date(inwds, format = "%Y")) %>%
      
      # Incorporate response from rlgblg to indicate if they have no religion
      mutate(rlgblg = haven::as_factor(rlgblg)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, rlgblg == "No", "No Religion"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know", "No answer"),
    "Other" = c("Other (nir)", "Altro", "Independent", "Autre", "Other")
  ),
  Language = list(
    "Missing" = c("777", "888", "999")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable", "Other Christian denomination", "Other Non-Christian religions", 
                "Other Christian, denomination not specified", "Other non-Christian religion, religion not specified",
                "Other protestant, denomination not specified", "Other Protestant", "Other non-Christian",
                "Other Eastern Orthodox", "Other Christian", "Other Christian Denominations",
                "Other Eastern Religions", "Other Eastern Orthodox, denomination not specified",
                "Other Non-Christian Religions", "Other Roman Catholic, denomination not specified",
                "Other Religions", "Other Catholic (Roman)", "Other Christian religions",
                "Other Christian denominations", "Other Christian denominations (Jehovah's Witnesses, Mormons, etc.)")
  )
)
