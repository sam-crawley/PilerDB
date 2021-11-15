data.spec <- list(
  file.name = "Divided/datasets/ess/Round 3/ESS3e03_7.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Party voted for in last national election [Which party did you vote for in that election?]",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  file.encoding = "latin1",
  skip.countries = list(),
  country.format = 'iso2c',
  field.def = c(
    "Party" = "prtvt",
    "Language" = "lnghoma",
    "Religion" = "rlgdnm",
    "Ethnicity" = NA,
    "Weight" = "pspwght",
    "Country" = "cntry",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "^prtv"), "prtvt")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2006)
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know","No answer", "Blanc", "Votou em branco / nulo",
                  "Blank vote", "Spoiled vote", "Blank", "Invalid"),
    "Other" = c("Autre", "Other", "Autres mouvements écologistes", "Independent", "Other party", "other", "Others")
  ),
  Language = list(
    "Missing" = c("777", "888", "999")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable", "Other non-Christian religions")
  )
)
