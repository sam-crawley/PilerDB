data.spec <- list(
  file.name = "ZA1950.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Australia" = "PartyVote",
    "Ireland" = "PartyVote",
    "Netherlands" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "D-W" = "Germany",
    "D-E" = "Germany",
    "gb" = "United Kingdom",
    "nirl" = "Northern Ireland",
    "usa" = "United States",
    "h" = "Hungary",
    "i" = "Italy",
    "irl" = "Ireland",
    "n" = "Norway",
    "s" = "Sweden",
    "il" = "Israel"
  ),
  field.def = c(
    "Party" = "v83",
    "Religion" = "v88",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v114"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    party_table <- list(
      Australia = list(
        `1` = "Liberal Party",
        `2` = "Australian Labour Party",
        `3` = "National Party",
        `4` = "Australian Democrats",
        `5` = "Greens",
        `6` = "Nuclear Disarmanent",
        `94` = "Other answer",
        `95` = "Other party",
        `96` = "None, no party",
        `99` = "NA"
      ),
      `Germany` = list(
        `1` = "CDU/ CSU",
        `2` = "SPD",
        `3` = "FDP",
        `4` = "Die Gruenen",
        `5` = "DKP",
        `6` = "NPD",
        `7` = "Alternative Liste",
        `8` = "SEW",
        `9` = "Republikaner",
        `94` = "Other answer",
        `95` = "Other party",
        `96` = "None, no party",
        `97` = "Refused",
        `98` = "Don't know, undecided",
        `99` = "NA"
      ),
      `United Kingdom` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "Democrat,SLD,Liberal",
        `4` = "SDP/ Social Democrat Party",
        `5` = "Alliance <Mainland>",
        `6` = "SNP/ Scottish Nationalist Party",
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
        `98` = "DK, undecided",
        `99` = "NA"
      ),
      'United States' = list(
        `1` = "Strong democrat",
        `2` = "Not strong democrat",
        `3` = "Independent, near democrat",
        `4` = "Independent",
        `5` = "Independent, near republican",
        `6` = "Not strong republican",
        `7` = "Strong republican",
        `95` = "Other party",
        `98` = "DK",
        `99` = "NA"
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
        `96` = "None",
        `0` = "Would not vote"
      ),
      Norway = list(
        `1` = "Red Electoral Alliance (R d Valgallianse)",
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
      
      mutate(Year = 1990)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("NA", "None", "Refused", "Don't know, undecided"),
    "Other" = c("Other answer", "Other party")
  ),
  Religion = list(
    "Missing" = c("NO DENOMINATION"),
    "Other" = c("OTHER RELIGION", "OTHER NON-CHRIST."),
    "No Religion" = c("none")
  )
)