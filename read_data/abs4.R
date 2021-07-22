# Party - q34
# Religion - se6
# Ethnicity - se11a
# Language - *Missing*

data.spec <- list(
  file.name = "Divided/data/asain barom/W4 Merged Data/W4_v15_merged20181211_release.dta",
  file.type = 'dta',
  skip.countries = c("China", "Vietnam"),
  country.format = 'country.name',
  field.def = c(
    "Party" = "q34",
    "Religion" = "se6",
    "Language" = NA,
    "Ethnicity" = "se11a",
    "Country" = "country",
    "Year" = "year",
    "Weight" = "w"
  ),
  fixups = function(data) {
    # Munge Singapore year (spans 2 years)
    data %>% mutate(Year = ifelse(Country == "Singapore" & Year == 2015, 2014, Year))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Not applicable", "Cannot recall", "Invalid vote", "Do not understand the question",
                  "Missing", "Can't choose", "Decline to answer")
  ),
  Religion = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer"),
    "Other" = c("Other", "Other Asian religions")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer")
  )
)