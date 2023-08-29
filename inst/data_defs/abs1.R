data.spec <- list(
  file.name = "Wave1_20170906.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Religion" = "What is your religion? [Exact question wording not supplied]"
  ),
  party.question.type = "PartyVote",
  country.party.question.type = c(
    "Taiwan" = "PresCand",
    "South Korea" = "PresCand"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q028",
    "Religion" = "se006",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "country",
    "Year" = NA,
    "Weight" = "w_all"
  ),
  fixups = function(data) {
    data %>% mutate(Year = case_when(
      Country == "Taiwan" ~ 2001,
      Country == "Philippines" ~ 2002,
      Country == "Mongolia" ~ 2002,
      Country == "Thailand" ~ 2001,
      Country == "Japan" ~ 2003,
      Country == "South Korea" ~ 2003,
      Country == "China" ~ 2001,
      TRUE ~ 0
    ))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "No answer", "Forgot", "Not affiliated"),
    "Other" = c("other", "Other parties")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("Others"),
    "No Religion" = c("None")
  )
)