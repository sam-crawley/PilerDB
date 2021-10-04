data.spec <- list(
  file.name = "Divided/datasets/ess/Round 8/ESS8e02_2.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Party voted for in last national election [Which party did you vote for in that election?]",
    "Religion" = "Do you consider yourself as belonging to any particular religion or denomination? [If yes] Which one?",
    "Language" = "What language or languages do you speak most often at home? [First mentioned]"
  ),  
  skip.countries = list(),
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
    
    rl.cols <- c("rlgdnbat", "rlgdnbe",  "rlgdnach", "rlgdnade", "rlgdnafi", "rlgdngb",  "rlgdnhu",  "rlgdnie",  "rlgdnis",
                 "rlgdnlt",  "rlgdnnl",  "rlgdnno",  "rlgdnapl",  "rlgdnase")
    
    data <- coalese.vars(data, rl.cols, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2016)
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know", "Don't know if voted for a multi-member nationwide candidate list", "No party",
                  "Blank vote", "No answer", "Did not vote for a multi-member nationwide candidate list",
                  "Blank paper", "White ballot", "Refused to tell if voted for a multi-member nationwide candidate list", "Invalid vote"),
    "Other" = c("Other (nir)", "Independent")
  ),
  Language = list(
    "Missing" = c("777", "888", "999", "ZXX")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable", "Other non-Christian religion, religion not specified", "Other non-Christian", "Other Non-Christian religions")
  )
)
