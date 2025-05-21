data.spec <- list(
  file.name = "ZA1840.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "[varies by country]",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?"
  ),
  party.question.type = "Closest",
  country.party.question.type = c(
    "Norway" = "PartyVote",
    "Netherlands" = "PartyVote"
  ),
  country.format = 'country.name',
  country.custom = c(
    "d" = "Germany",
    "gb" = "United Kingdom",
    "nirl" = "Northern Ireland",
    "usa" = "United States",
    "a" = "Austria",
    "h" = "Hungary",
    "i" = "Italy",
    "irl" = "Ireland",
    "n" = "Norway",
    "nl" = "Netherlands",
    "il" = "Israel"
  ),
  field.def = c(
    "Party" = "v103",
    "Religion" = "v108",
    "Language" = NA,
    "Ethnicity" = NA,
    "Country" = "v3",
    "Year" = NA,
    "Weight" = "v136"
  ),
  fixups = function(data) {
    # Same codes used for different parties (in different countries)
    #  So we need to recode them all based on the codebook
    party_table <- list(
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
      `United States` = list(
        `1` = "Strong democrat",
        `2` = "Not strong democrat",
        `3` = "Independent, near democrat",
        `4` = "Independent",
        `5` = "Independent, near republican",
        `6` = "Not strong republican",
        `7` = "Strong republican",
        `95` = "Other party",
        `98` = "DK",
        `99` = "NA",
        `0` = "NAP"
      ),
      Austria = list(
        `1` = "Socialist Party of Austria",
        `2` = "Austrian Peoples Party",
        `3` = "Freedom Party",
        `4` = "Communist Party",
        `5` = "United Ecologists of Austria",
        `6` = "Alternative List of Austria",
        `97` = "None",
        `99` = "NA"
      ),
      `Northern Ireland` = list(
        `1` = "Conservative",
        `2` = "Labour",
        `3` = "Democrat, SLD, Liberal",
        `4` = "SDP (Social Democrat)",
        `5` = "Alliance <Mainland>",
        `6` = "SNP",
        `8` = "Green party",
        `10` = "Alliance (Northern Ireland)",
        `11` = "Democratic Unionist Party (DUP)",
        `12` = "Official/Ulster Unionist Party (OUP)",
        `13` = "Other Unionists",
        `14` = "Sinn Fein",
        `15` = "SDLP",
        `16` = "Workers Party",
        `95` = "Other party",
        `96` = "None",
        `97` = "Refused",
        `98` = "DK, undecided",
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
        `1` = "Progress party (to the right of Conservatives)",
        `2` = "Conservatives",
        `3` = "Centre party (former peasant's party)",
        `4` = "Christians people party",
        `5` = "Liberals",
        `6` = "Labour",
        `7` = "Socialist left",
        `8` = "Communists",
        `9` = "Coalition of Marxists-Leninists, independent socialists and anarchists",
        `98` = "Don't know, would not vote",
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
      mutate(Party = if_else(Country %in% c("United States"), "Missing", Party)) %>%
      
      mutate(Year = 1989)
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("NA", "None", "Refused", "Don't know, undecided"),
    "Other" = c("Other party")
  ),
  Religion = list(
    "Missing" = c("NO DENOMINATION"),
    "Other" = c("OTHER NON-CHRIST."),
    "No Religion" = c("none")
  )
)