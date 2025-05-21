data.spec <- list(
  file.name = "ZA1700.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which political party do you most agree with?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.format = 'country.name',
  country.custom = c(
    "d" = "Germany",
    "gb" = "United Kingdom",
    "usa" = "United States",
    "a" = "Austria",
    "h" = "Hungary",
    "i" = "Italy",
    "irl" = "Ireland",
    "nl" = "Netherlands"
  ),
  field.def = c(
    "Party" = "v84",
    "Religion" = "v91",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v116"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    # Note: party only available in Austria for this wave
    #  (And US, but it uses the strong/lean(etc) Democratic/Republican)
    party_table <- list(
      Austria = list(
        `1` = "Socialist Party of Austria",
        `2` = "Austrian Peoples Party",
        `3` = "Freedom Party",
        `4` = "Communist Party",
        `5` = "United Ecologists of Austria",
        `6` = "Alternative List of Austria",
        `97` = "None",
        `99` = "NA"
      )
    )

    data %>% 
       mutate(
         Party = purrr::map2_chr(
           Country, Party,
           ~ purrr::pluck(party_table, .x, .y, .default = "Missing")
         )
       ) %>%
       mutate(Year = 1989)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("None"),
    "Other" = c()
  ),
  Religion = list(
    "Missing" = c("NO DENOMONAT."),
    "Other" = c("OTHER RELIG", "OTHER NON CHRIST"),
    "No Religion" = c("none")
  )
)