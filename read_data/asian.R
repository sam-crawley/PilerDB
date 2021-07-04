library(haven)

# Party - q34
# Religion - se6
# Ethnicity - se11a
# Language - *Missing*

asb4.cats <- list(
  Party = list(
    "Missing" = c("Not applicable", "Cannot recall", "Invalid vote", "Do not understand the question",
                  "Missing", "Can't choose", "Decline to answer")
  ),
  Religion = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer")
  ),
  Ethnicity = list(
    "Missing" = c("Missing", "Can't choose", "Decline to answer")
  )
)

asian.w4.skip.countries <- c("China", "Vietnam")

read.data.asian <- function() {
  data <- read_dta("Divided/data/asain barom/W4 Merged Data/W4_v15_merged20181211_release.dta")
  
  data <- data %>%
    mutate(across(c(q34, se6, se11a, ir2c), haven::as_factor)) %>%
    mutate(across(c(q34, se6, se11a, ir2c), ~fct_explicit_na(.x, na_level = "Missing"))) %>%
    rename(
      "Party" = q34,
      "Religion" = se6,
      "Ethnicity" = se11a,
      "Country" = country,
      "Year" = year
    ) %>%
    mutate(Language = factor("Missing")) %>%
    mutate(Country = as.character(haven::as_factor(Country))) %>%
    filter(! Country %in% asian.w4.skip.countries) %>%
    arrange(Country)
  
  # Munge Singapore year (spans 2 years)
  data <- data %>% mutate(Year = ifelse(Country == "Singapore" & Year == 2015, 2014, Year))
  
  data
  
}