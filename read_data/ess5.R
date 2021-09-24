data.spec <- list(
  file.name = "Divided/datasets/ess/Round 5/ESS5e03_4.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Party voted for in last national election [Which party did you vote for in that election?]",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  file.encoding = "latin1",
  skip.countries = list(
    no_group = c("Cyprus", "Czechia", "Denmark", "Greece", "Poland", "Slovenia", "Sweden")
  ),
  country.format = 'iso2c',
  field.def = c(
    "Party" = "prtvt",
    "Language" = "lnghom1",
    "Religion" = "rel",
    "Ethnicity" = NA,
    "Weight" = "pspwght",
    "Country" = "cntry",
    "Year" = NA
  ),
  pre_fixups = function(data) {
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "^prtv"), "prtvt")
    
    rl.cols <- str_subset(names(data), "^rlgdn") %>%
      discard(~.x == "rlgdnme")
    
    data <- coalese.vars(data, rl.cols, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2010)
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know","No answer", "Nul", "Blanc", "Does not know if voted for a candidate list", "Ongeldig",
                  "A white ballot (empty ballot note)", "Blank paper", "Votou em branco / nulo", "Blanco", "Blank vote", "Spoiled vote", "Did not vote"),
    "Other" = c("Autre", "Outro", "Other", "Autres mouvements écologistes", "Independent",
                "Mixed vote", "Did not vote for a candidate list")
  ),
  Language = list(
    "Missing" = c("777", "888", "999", "ZXX")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable", "Other non-Christian religions")
  )
)
