data.spec <- list(
  file.name = "Divided/datasets/arab/Wave 5/ABV_Release_Data.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party if any do you feel closest to?",
    "Religion" = "What is your religious denomination?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q503A",
    "Language" = NA,
    "Religion" = "Q1012A",
    "Ethnicity" = NA,
    "Weight" = "wt",
    "Country" = "country",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(date, format = "%Y")) %>%
      mutate(Year = if_else(is.na(Year), "2018", Year))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("no party", "Missing", "don't know", "refused"),
    "Other" = c("other")
  ),
  Religion = list(
    "Missing" = c("Missing", "don't know", "refused"),
    "Other" = c("other")
  )
)
