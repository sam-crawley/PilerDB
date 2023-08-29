data.spec <- list(
  file.name = "ZA4800_v4-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party would you vote for?",
    "Religion" = "Do you belong to a religious denomination? [If yes] Which one?"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
  country.custom = c(
    "CY-TCC" = "Cyprus",
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom",
    "RS-KM" = "Kosovo"
  ),
  field.def = c(
    "Party" = "v264",
    "Language" = NA,
    "Religion" = "v106_cs",
    "Ethnicity" = NA,
    "Weight" = "weight_g",
    "Country" = "c_abrv",
    "Year" = "year"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\s*[\\w-]+:\\s*")
    }
    
    # Incorporate response from v105 to indicate if they have no religion
    data <- data %>% mutate(v105 = haven::as_factor(v105)) %>%
      mutate(Religion = fct_expand(Religion, "No Religion")) %>% 
      mutate(Religion = replace(Religion, v105 == "no", "No Religion"))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Blank", "Invalid", "Empty ballot", "Missing", "Would cast a blank vote"),
    "Other" = c("Other", "Other Left Wing Parties (Radical Leftist Party, Republicain's and Citizen's Movement)",
                "Independents")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("Other", "Une autre religion")
  )
)
