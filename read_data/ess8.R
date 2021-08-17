data.spec <- list(
  file.name = "Divided/data/ess/Round 8/ESS8e02_2.dta",
  file.type = 'dta',
  skip.countries = c('Portugal'),
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
                  "Blank vote", "No answer", "Nul", "Blanc", "Did not vote for a multi-member nationwide candidate list", "Invalid ballot")
  ),
  Language = list(
    "Missing" = c("777", "888", "999", "ZXX")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer"),
    "Other" = c("Not applicable", "Other non-Christian religion, religion not specified", "Other non-Christian")
  )
)
