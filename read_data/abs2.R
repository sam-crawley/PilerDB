data.spec <- list(
  file.name = "Divided/datasets/asain barom/W2 Merged Data/2w-3rd_release_all/merge/Wave2_20170724.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  question.text = c(
    "Religion" = "What is your religion?"
  ),
  skip.countries = list(
    no_party = c("Cambodia", "China", "Singapore", "Vietnam")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q39",
    "Religion" = "se6",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "country",
    "Year" = NA,
    "Weight" = "w_all"
  ),
  fixups = function(data) {
    data %>% mutate(Year = case_when(
      Country == "Taiwan" ~ 2006,
      Country == "Philippines" ~ 2005,
      Country == "Mongolia" ~ 2006,
      Country == "Thailand" ~ 2006,
      Country == "Malaysia" ~ 2007,
      Country == "Indonesia" ~ 2006,
      Country == "Vietnam" ~ 2005,
      Country == "Japan" ~ 2007,
      Country == "South Korea" ~ 2006,
      Country == "Cambodia" ~ 2008,
      TRUE ~ 0
    ))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Decline to answer", "No answer", "Forgot", "Do not know", "Cannot choose", "no information",
                  "voted in blank", "Vote for no vote", "Non-valid vote"),
    "Other" = c("other party", "Independent", "Other Party", "Opposition", "Other", "Other Parties")
  ),
  Religion = list(
    "Missing" = c("Missing", "Decline to answer", "Cannot choose"),
    "Other" = c("Other", "None")
  )
)