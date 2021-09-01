data.spec <- list(
  file.name = "Divided/data/ess/Round 7/ESS7e02_2.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  skip.countries = c(),
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
    
    rl.cols <- c("rlgdnbat", "rlgdnbe",  "rlgdnach", "rlgdnade", "rlgdnafi", "rlgdngb",  "rlgdnhu",  "rlgdnie", "rlgdnil",
                 "rlgdnlt",  "rlgdnnl",  "rlgdnno",  "rlgdnapl",  "rlgdnase", "rlgdnsi")
    
    data <- coalese.vars(data, rl.cols, "rel")
    
    data
  },
  fixups = function(data) {
    data %>% mutate(Year = 2014)
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Refusal", "Don't know","No answer", "Nul", "Blanc", "Does not know if voted for a candidate list", "Blanco",
                  "Ongeldig", "Blank paper", "White ballot", "Votou em branco / nulo", "Votó en blanco", "Votó nulo"),
    "Other" = c("Andet - other", "Otros", "Autre", "Other (nir)", "Outro", "Other party", "Other", "Did not vote for a candidate list")
  ),
  Language = list(
    "Missing" = c("777", "888", "999")
  ),
  Religion = list(
    "Missing" = c("Refusal", "No answer", "Missing"),
    "Other" = c("Not applicable", "Other non-Christian religion, religion not specified", "Other non-Christian", "Other Religions",
                "Other Non-Christian Religions", "Other non-Christian religions")
  )
)
