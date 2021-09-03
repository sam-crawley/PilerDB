data.spec <- list(
  file.name = "Divided/data/ess/Round 9/ESS9e03_1.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Party voted for in last national election [Which party did you vote for in that election?]",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  skip.countries = list(
    no_group = c("Croatia", "Cyprus", "Czechia", "Denmark", "France", "Iceland", "Ireland", "Portugal")
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
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "^prtv"), "prtvt")
    
    rl.cols <- c("rlgdnbat", "rlgdnbe",  "rlgdnach", "rlgdncy",  "rlgdnade", "rlgdnafi", "rlgdngb",  "rlgdnhu",  "rlgdnie",  "rlgdnais",
                 "rlgdnlt",  "rlgdnlv",  "rlgdme",   "rlgdnnl",  "rlgdnno",  "rlgdnapl", "rlgdnrs",  "rlgdnase", "rlgdnask", "rlgdnme")
    
    data <- coalese.vars(data, rl.cols, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2018)
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know", "Invalid", "Don't know if voted for a multi-member nationwide candidate list", "No answer",
                  "Blank vote", "Invalid vote", "Did not vote for a multi-member nationwide candidate list",
                  "Refused to tell if voted for a multi-member nationwide candidate list", "Blanc", "Blank paper"),
    "Other" = c("Independent(s) (nir)", "Other (nir)", "Altro")
  ),
  Language = list(
    "Missing" = c("777", "888", "999", "ZXX")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable", "Other non-Christian religion, religion not specified", "Other non-Christian", "Other Non-Christian Religions",
                "Other Non-Christian religions")
  )
)
