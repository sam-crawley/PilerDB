library(haven)

# Q99 - Party
# Q98 - Religion
# Q2A - Language
# Q84 - Ethnicity

read.data.afro <- function() {
  
  data <- read_sav("Divided/data/afrobarom/r7_merged_data_34ctry.release.sav")
  
  data <- data %>%
    mutate(across(c(Q99, Q98, Q2A, Q84, COUNTRY), haven::as_factor)) %>%
    mutate(across(c(Q99, Q98, Q2A, Q84), fct_explicit_na)) %>%
    filter(COUNTRY != "eSwatini") %>%
    rename(
      "Party" = Q99,
      "Language" = Q2A,
      "Religion" = Q98,
      "Ethnicity" = Q84,
      "Country" = COUNTRY
    ) %>%
    mutate(Year = 2018) %>%
    mutate(Party = fct_collapse(Party,
       "None/Missing/DK" = c("Missing", "Would not vote", "Refused", "Don't know", 
       "Not asked in the country")
    )) %>%
    mutate(across(c(Religion, Ethnicity, Language), ~fct_relabel(.x, ~str_replace(.x, "Missing", "\\(Missing\\)"))))
  
  return(data)
}