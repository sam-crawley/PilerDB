data.spec <- list(
  file.name = "ZA2150.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "PartyVote",
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "D-W" = "Germany",
    "D-E" = "Germany",
    "gb" = "United Kingdom",
    "nirl" = "Northern Ireland",
    "usa" = "United States",
    "a" = "Austria",
    "h" = "Hungary",
    "i" = "Italy",
    "irl" = "Ireland",
    "n" = "Norway",
    "s" = "Sweden",
    "cz" = "Czechia",
    "slo" = "Slovenia",
    "pl" = "Poland",
    "il" = "Israel",
    "rus" = "Russia",
    "nz" = "New Zealand",
    "nl" = "Netherlands",
    "rp" = "Philippines"
  ),
  field.def = c(
    "Party" = "v101",
    "Religion" = "v106",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v131"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    party_table <- list(
      Germany = list(
        `1` = "CDU/CSU",
        `2` = "SPD",
        `3` = "FDP",
        `4` = "Die Gruenen/Buendnis 90",
        `5` = "DKP",
        `6` = "NPD",
        `7` = "Republikaner",
        `8` = "PDS/Linke Liste",
        `94` = "Other answer",
        `95` = "Other party",
        `96` = "None, no party",
        `97` = "Refused",
        `98` = "Don't know, undecided",
        `99` = "NA"
      ),
      `Great Britain` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "Democrat,SLD,Liberal",
        `4` = "SDP/Social Democrat Party",
        `5` = "Alliance <Mainland>",
        `6` = "SNP/Scottish Nationalist Party",
        `7` = "Plaid Cymru",
        `8` = "Green party",
        `94` = "Other answer",
        `95` = "Other party",
        `96` = "None",
        `97` = "Refused",
        `98` = "Don't know, undecided",
        `99` = "NA"
      ),
      `Northern Ireland` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "SDP (Social Democrat, SLD, Liberal, Alliance <Mainland>",
        `4` = "Alliance (Northern Ireland)",
        `5` = "Democratic Unionist Party (DUP)",
        `6` = "Official/Ulster Unionist Party (OUP)",
        `7` = "SDLP",
        `8` = "Green party",
        `9` = "Sinn Fein",
        `10` = "Workers Party",
        `94` = "Other answer",
        `95` = "Other party",
        `96` = "None",
        `97` = "Refused",
        `98` = "Undecided",
        `99` = "DK, NA"
      ),
      Ireland = list(
        `1` = "Fianna Fail",
        `2` = "Fine Gael",
        `3` = "Labour",
        `4` = "Workers party",
        `5` = "Progressive party",
        `6` = "Green Party",
        `7` = "Sinn Fein",
        `95` = "Other party",
        `96` = "None, would not vote",
        `99` = "NA"
      ),
      Norway = list(
        `1` = "Red Electoral Alliance",
        `2` = "Labour Party",
        `3` = "Progress Party",
        `4` = "Conservative Party",
        `5` = "Christian Democratic Party",
        `6` = "Centre party",
        `7` = "Socialist Left Party",
        `8` = "Liberal Party",
        `95` = "Other parties",
        `96` = "None, would not vote",
        `98` = "Don't know",
        `99` = "NA"
      ),
      Slovenia = list(
        `1` = "Democrat party",
        `2` = "Christian socialists",
        `3` = "Liberal democrat party",
        `4` = "Liberal party",
        `5` = "SDZ - National democrat party",
        `6` = "Social democratic union",
        `7` = "Social democratic alliance of Slovenia",
        `8` = "Socialist party of Slovenia",
        `9` = "Slovenian peoples party",
        `10` = "Slovenian Christian democrats",
        `11` = "Social democratic reconstruction",
        `12` = "Green of Slovenia",
        `95` = "Other parties",
        `96` = "Would not vote",
        `99` = "Don't know"
      ),
      Hungary = list(
        `1` = "Hungary Democratic Forum",
        `2` = "Alliance of Free Democrats",
        `3` = "Independant Small Holders Party",
        `4` = "Hungarian Socialist Party",
        `5` = "Alliance of Young Democrats",
        `6` = "Christian Democratic Peoples Party",
        `7` = "Hungarian Socialist Workers' Party",
        `95` = "Other party",
        `96` = "Would not vote",
        `99` = "NA"
      ),
      Poland = list(
        `1` = "Democratic Union",
        `2` = "Left-Democracy Alliance",
        `3` = "Catholic Election Action",
        `4` = "Polish Peasant Party Programme Alliance",
        `5` = "Independent Poland Confederation",
        `6` = "Center Alliance",
        `7` = "Liberal-Democratic Alliance",
        `8` = "Election Committee Peasant Alliance",
        `9` = "Solidarity Trade Union",
        `10` = "Polish Beer Friends Party",
        `11` = "Real Politic Union",
        `12` = "Solidarity of Work",
        `13` = "National Party",
        `14` = "Coalition: Polish Green Party and Polish Ecological Party",
        `15` = "German Minority",
        `16` = "In Solidarity with the President",
        `95` = "Other parties",
        `96` = "No Party, no preference",
        `98` = "Don't know",
        `99` = "NA"
      ),
      Russia = list(
        `1` = "Democratic Russia, democrats",
        `2` = "Democratic party of Russia",
        `3` = "Democratic union tolicka",
        `4` = "Green movement",
        `5` = "Communist party",
        `6` = "Communists for democracy",
        `7` = "Liberals, Liberal-democratic party",
        `8` = "Social-democrats",
        `9` = "Christian democrats",
        `10` = "Anarchists",
        `11` = "Pamyat",
        `12` = "Russian Communist party",
        `94` = "Other parties",
        `95` = "Candidates not belonging to any party",
        `99` = "Unclear answer, no answer"
      ),
      Austria = list(
        `1` = "SPOE (Social-Democrats)",
        `2` = "OEVP (Conservative Party)",
        `3` = "FPOE (Liberal Party)",
        `4` = "GA (left-wing Green Party)",
        `5` = "VGOE (conservative Green Party)",
        `6` = "KPOE (Communist Party)",
        `95` = "Other Party",
        `96` = "None, no party",
        `99` = "NA"
      ),
      Australia = list(
        `1` = "Liberal party",
        `2` = "Australian Labour party",
        `3` = "National Party",
        `4` = "Australian Democrats",
        `5` = "Greens",
        `6` = "Nuclear Disarmament",
        `95` = "Other Party",
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
      mutate(Country = if_else(Country == "Northern Ireland", "United Kingdom", Country)) %>%
      
      mutate(Year = 1991) %>%
      
      # Ignore weights for Russia, since some are 0
      mutate(Weight = if_else(Country == "Russia", 1, Weight)) %>%
    
      # All weights for Australia are 0. Set to 1 instead
      mutate(Weight = if_else(Country == "Australia", 1, Weight)) %>%
      
      mutate(Religion = if_else(Religion == 54, "Orthodox", Religion)) %>%
      mutate(Religion = if_else(Religion == 60, "Free Presbyterian", Religion))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("NA", "Don't know", "No Party, no preference"),
    "Other" = c("Other answer")
  ),
  Religion = list(
    "Missing" = c("No denom given"),
    "Other" = c("Other non-christian", "Other religion"),
    "No Religion" = c("None")
  )
)