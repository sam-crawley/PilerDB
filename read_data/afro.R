library(haven)

# Q99 - Party
# Q98 - Religion
# Q2A - Language
# Q84 - Ethnicity

afro7.cats <- list(
  Party = list(
    "Missing" = c("Missing", "Would not vote", "Refused", "Don't know", "Not asked in the country")
  ),
  Language = list(
    "Missing" = c("Missing", "Refused To Answer", "Don't know")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None")
  ),
  Ethnicity = list(
    "Other" = c("Other", "Doesn\u2019t think of self in those terms",
                "Related to regional origin (badio/sampadjudo)",
                "Related to Gender",
                "Related to Religion",
                "Related to Race",
                "Related to political-partisan affiliation",
                "Related to social groups (Foros, Angulares, Cabo-verdianos, Principenses)",
                "Related to age",
                "Related to gender",
                "Related to the job",
                "Related to religian",
                "Related to social classes",
                "Related to race",
                "Related to political party affiliation"),
    "Missing" = c("Missing", "Not asked in the country", "Refused", "Don't know")
  )
)

read.data.afro <- function() {
  
  data <- read_sav("Divided/data/afrobarom/r7_merged_data_34ctry.release.sav")
  
  data <- data %>%
    mutate(across(c(Q99, Q98, Q2A, Q84), haven::as_factor)) %>%
    mutate(across(c(Q99, Q98, Q2A, Q84), ~fct_explicit_na(.x, na_level = "Missing"))) %>%
    mutate(Country = countrycode(as.character(haven::as_factor(COUNTRY)), origin = "country.name", destination = "country.name")) %>%
    filter(Country != "Eswatini") %>%
    rename(
      "Party" = Q99,
      "Language" = Q2A,
      "Religion" = Q98,
      "Ethnicity" = Q84
    ) %>%
    mutate(Year = 2018) %>%

  return(data)
}