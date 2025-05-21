data.spec <- list(
  file.name = "ZA1620.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "What is your religious denomination? [varies by country]"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Germany" = "PartyVote",
    "United Kingdom" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "d" = "Germany",
    "gb" = "United Kingdom",
    "usa" = "United States",
    "a" = "Austria",
    "h" = "Hungary",
    "i" = "Italy"
  ),
  field.def = c(
    "Party" = "v95",
    "Religion" = "v96",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v105"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    party_table <- list(
      Australia = list(
        `1` = "Liberal Party",
        `2` = "Australian Labor Party",
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
        `1` = "Christlich Demokratische Union (CDU)/ Christlich Soziale Union (CSU)",
        `2` = "Sozialdemokratische Partei Deutschlands (SPD)",
        `3` = "Freie Demokratische Partei (FDP)",
        `4` = "Nationaldemokratische Partei (NPD)",
        `5` = "Kommunistische Partei Deutschlands (DKP)",
        `6` = "Die Gruenen",
        `95` = "Other party",
        `96` = "Would not vote",
        `97` = "Refused",
        `98` = "Don't know",
        `99` = "NA",
        `0` = "Not applicable (living in Berlin)"
      ),
      `United Kingdom` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "Liberal Alliance",
        `4` = "Social Democratic Party (SDP)",
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
        `95` = "Other party, refused to say",
        `96` = "Other answer",
        `98` = "Don't know",
        `99` = "NA"
      ),
      Austria = list(
        `1` = "Sozialistische Partei Oesterreichs (SPOE)",
        `2` = "Oesterreichische Volkspartei (OEVP)",
        `3` = "Freiheitliche Partei Oesterreichs (FPOE)",
        `4` = "Vereinte Gruene Oesterreichs (VGOE)",
        `5` = "Alternative Liste Oesterreichs (ALOE)",
        `6` = "Buergerinitiative Parlament (BIP)",
        `7` = "Kommunistische Partei Oesterreichs (KPOE)",
        `95` = "Other party, refused to say",
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
      
      # Set all Hungary's weights to 1, since some are 0
      mutate(Weight = if_else(Country == "Hungary", 1, Weight)) %>%
      
      mutate(Year = 1986)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Don't know", "None", "Don't know, unsure", "NA", "NA, refused",
                  "Refused", "Would not vote"),
    "Other" = c("Other party", "Other answer", "Other party, refused to say")
  ),
  Religion = list(
    "Missing" = c("No denomination"),
    "Other" = c("country Specific", "Oth christ a non-christ denom"),
    "No Religion" = c("None")
  )
)