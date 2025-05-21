data.spec <- list(
  file.name = "ZA1490.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "What is your religious denomination? [varies by country]"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "United Kingdom" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "aus" = "Australia",
    "d" = "Germany",
    "gb" = "United Kingdom",
    "usa" = "United States",
    "a" = "Austria",
    "i" = "Italy"
  ),
  field.def = c(
    "Party" = "V131",
    "Religion" = "V132",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "V3",
    "Year" = NA,
    "Weight" = "V141"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    party_table <- list(
      Australia = list(
        `1` = "Liberal Party",
        `2` = "Australian Labour Party",
        `3` = "National (Country) Party",
        `4` = "Australian Democrats",
        `5` = "Democratic Labour Party",
        `6` = "Nuclear Disarmament Party",
        `95` = "Other party",
        `96` = "Other answer",
        `97` = "None",
        `98` = "Don't know",
        `99` = "No answer, refused"
      ),
      Germany = list(
        `1` = "Sozialdemokratische Partei Deutschlands (SPD)",
        `2` = "Christlich Demokratische Union (CDU)",
        `3` = "Christlich Soziale Union (CSU)",
        `4` = "Freie Demokratische Partei (FDP)",
        `5` = "Gruene",
        `95` = "Other party",
        `96` = "Other answer",
        `97` = "None",
        `98` = "Don't know",
        `99` = "No answer"
      ),
      `United Kingdom` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "Liberal Alliance",
        `4` = "Social Democratic Party (SDP)",
        `5` = "Alliance (Liberal/ Social Democratic Alliance)",
        `6` = "Scottish Nationalists",
        `7` = "Plaid Cymru",
        `95` = "Other party",
        `96` = "Other answer",
        `97` = "None",
        `98` = "Don't know",
        `99` = "No answer"
      ),
      `United States` = list(
        `1` = "Strong democrat",
        `2` = "Not strong democrat",
        `3` = "Independent, near democrat",
        `4` = "Independent, near republican",
        `5` = "Not strong republican",
        `6` = "Strong republican",
        `95` = "Other party, refused to say",
        `96` = "Other answer",
        `97` = "Independent (neither, no response)",
        `98` = "Don't know",
        `99` = "No answer"
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
        `99` = "No answer"
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
      #mutate(Weight = if_else(Country == "Hungary", 1, Weight)) %>%
      
      mutate(Year = 1985)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Don't know", "None", "No answer", "No answer, refused"),
    "Other" = c("Other party", "Other answer", "Other party, refused to say")
  ),
  Religion = list(
    "Missing" = c("NO DENOMINATION GIVE"),
    "Other" = c("other"),
    "No Religion" = c("none")
  )
)