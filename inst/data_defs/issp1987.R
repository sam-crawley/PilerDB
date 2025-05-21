data.spec <- list(
  file.name = "ZA1680.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "What is your religious denomination? [varies by country]"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Australia" = "PartyVote",
    "Netherlands" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "d" = "Germany",
    "gb" = "United Kingdom",
    "usa" = "United States",
    "a" = "Austria",
    "h" = "Hungary",
    "nl" = "Netherlands",
    "i" = "Italy",
    "ch" = "Switzerland",
    "pl" = "Poland"
  ),
  field.def = c(
    "Party" = "v97",
    "Religion" = "v98",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v107"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    party_table <- list(
      Australia = list(
        `1` = "Liberal Party",
        `2` = "Australian Labor Party - ALP",
        `3` = "National (Country) Party",
        `4` = "Australian Democrats",
        `5` = "Democratic Labor Party",
        `6` = "Nuclear Disarmament Party",
        `95` = "Other party",
        `96` = "Other answer",
        `97` = "None, no party",
        `98` = "Don't know, unsure",
        `99` = "NA, refused"
      ),
      Germany = list(
        `1` = "Social Democrats",
        `2` = "Christian Democratic Union",
        `3` = "Christian Social Union",
        `4` = "Free Democrats",
        `5` = "Ecologists",
        `95` = "Other party",
        `97` = "None",
        `98` = "Refused, don't know",
        `99` = "NA"
      ),
      `United Kingdom` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "Liberal Alliance",
        `4` = "Social Democratic Party - SDP",
        `5` = "Alliance",
        `6` = "Scottish Nationalists",
        `7` = "Plaid Cymru",
        `95` = "Other party",
        `96` = "Other answer",
        `97` = "None",
        `98` = "Don't know",
        `99` = "NA"
      ),
      `United States` = list(
        `1` = "Strong Democrat",
        `2` = "Not strong Democrat",
        `3` = "Independent, near Democrat",
        `4` = "Independent",
        `5` = "Independent, near Republican",
        `6` = "Not strong Republican",
        `7` = "Strong Republican",
        `95` = "Other party",
        `98` = "Don't know",
        `99` = "NA"
      ),
      Austria = list(
        `1` = "Socialist Party of Austria",
        `2` = "Austrian Peoples Party",
        `3` = "Freedom Party",
        `4` = "Communist Party",
        `5` = "United Ecologists of Austria",
        `6` = "Alternative List of Austria",
        `97` = "None",
        `98` = "Don't know",
        `99` = "NA"
      ),
      Netherlands = list(
        `1` = "Labour Party",
        `2` = "Peoples Party for Freedom and Democracy: Liberal Party",
        `6` = "Christian Democratic Appeal",
        `7` = "Political Radical Party",
        `8` = "Democratic Party 66",
        `9` = "Pacifist Socialist Party",
        `10` = "Communist Party of the Netherlands",
        `13` = "Constitutional Reformed Protestant Party",
        `14` = "Reformed Political Union",
        `15` = "Reformed Protestant Political Federation",
        `16` = "Centrum Party",
        `17` = "Evangelical Political Party"
      ),
      Switzerland = list(
        `1` = "Radical Democrats",
        `2` = "Christian Democratic Peoples Party",
        `3` = "Social Democrats",
        `4` = "Swiss Peoples Party",
        `5` = "Independents Party",
        `6` = "Liberal Conservatives",
        `7` = "Protestant Peoples Party",
        `8` = "Labour Party",
        `9` = "Progressive Organisations of Switzerland",
        `10` = "Ecologists",
        `11` = "National Action",
        `12` = "Conservative Christian Social Party",
        `95` = "Other party",
        `96` = "Other answer",
        `97` = "None",
        `98` = "Don't know",
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
      mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      
      mutate(Religion = if_else(Religion == 47, "Uniting Church", Religion)) %>%
      mutate(Religion = if_else(Religion == 48 & Country == "Germany", "Protestant (evangelische) free church", Religion)) %>%
      mutate(Religion = if_else(Religion == 48 & Country == "Netherlands", "Orthodox protestant church", Religion)) %>%
      mutate(Religion = if_else(Religion == 54, "Orthodox", Religion)) %>%
      
      mutate(Year = 1987)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Don't know", "None"),
    "Other" = c("Other party", "Other answer")
  ),
  Religion = list(
    "Missing" = c("NO DENOM"),
    "Other" = c("Other christ, non-christ"),
    "No Religion" = c("none")
  )
)