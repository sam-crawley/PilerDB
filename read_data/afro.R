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
    "Missing" = c("(Missing)", "Refused To Answer", "Don't know")
  ),
  Religion = list(
    "Missing" = c("(Missing)", "Don't know", "Refused")
  ),
  Ethnicity = list(
    "Other" = c("Other", "Doesn’t think of self in those terms"),
    "Missing" = c("(Missing)", "Not asked in the country", "Refused", "Don't know")
  )
)

read.data.afro <- function() {
  
  data <- read_sav("Divided/data/afrobarom/r7_merged_data_34ctry.release.sav")
  
  data <- data %>%
    mutate(across(c(Q99, Q98, Q2A, Q84), haven::as_factor)) %>%
    mutate(across(c(Q99, Q98, Q2A, Q84), fct_explicit_na)) %>%
    mutate(COUNTRY = as.character(haven::as_factor(COUNTRY))) %>%
    filter(COUNTRY != "eSwatini") %>%
    rename(
      "Party" = Q99,
      "Language" = Q2A,
      "Religion" = Q98,
      "Ethnicity" = Q84,
      "Country" = COUNTRY
    ) %>%
    mutate(Year = 2018) %>%
    mutate(across(c(Religion, Ethnicity, Language), ~fct_relabel(.x, ~str_replace(.x, "Missing", "\\(Missing\\)"))))

  return(data)
}