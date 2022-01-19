data.spec <- list(
  file.name = "Divided/datasets/cses/Module 5/cses5.dta",
  file.type = 'dta',
  split.by.year = T,
  country.format = 'country.name',
  question.text = c(
    "Party" = "Which party do you feel closest to?",
    "Language" = "Language usually spoken at home",
    "Religion" = "Religious denomination"
  ),  
  field.def = c(
    "Party" = "E3024_3",
    "Language" = "E2019",
    "Religion" = "E2013",
    "Ethnicity" = NA,
    "Weight" = "E1010_2",
    "Country" = "E1006_NAM",
    "Year" = "E1008"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    levels(data$Party) <- str_remove(levels(data$Party), "^\\d+\\. \\w+ - \\s*")
    
    # Deal with the country-specific recodes for Language/Religion
    data <- do.country.recodes(data)
    
    for (var in c("Language", "Religion")) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\d+\\. ")
    }
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("999997. VOLUNTEERED: REFUSED", "999998. VOLUNTEERED: DON'T KNOW", "999999. MISSING"),
    "Other" = c("999989. INDEPENDENT CANDIDATE", "999992. OTHER CANDIDATE/PARTY (NOT FURTHER SPECIFIED)")
  ),
  Language = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("[SEE ELECTION STUDY NOTES]", "OTHER: NOT SPECIFIED")
  ),
  Religion = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("INDEPENDENT, OTHER [SEE ELECTION STUDY NOTES]", "[SEE ELECTION STUDY NOTES]", "OTHER: NOT SPECIFIED"),
    "No Religion" = c("AGNOSTIC", "ATHEIST", "NONE")
  )
)

country.recodes <- list(
  Language = list(
    "Brazil|2018" = list(
      "Esperanto" = "980. [SEE ELECTION STUDY NOTES]",
      "Indigenous language / \"caipanga\" / \"tupi guarani\"" = "981. [SEE ELECTION STUDY NOTES]"
    ),
    "Iceland|2016" = list(
      "Icelandic and English" = "980. [SEE ELECTION STUDY NOTES]",
      "Faroese" = "981. [SEE ELECTION STUDY NOTES]",
      "Sign Language" = "982. [SEE ELECTION STUDY NOTES]",
      "Icelandic and Bulgarian" = "983. [SEE ELECTION STUDY NOTES]",
      "Icelandic and Danish" = "984. [SEE ELECTION STUDY NOTES]",
      "Icelandic and Norwegian" = "985. [SEE ELECTION STUDY NOTES]"
    ),
    "Iceland|2017" = list(
      "Nepalese" = "980. [SEE ELECTION STUDY NOTES]",
      "Faroese" = "981. [SEE ELECTION STUDY NOTES]"
    ),
    "New Zealand|2017" = list(
      "Amharic" = "980. [SEE ELECTION STUDY NOTES]",
      "Khmer" = "981. [SEE ELECTION STUDY NOTES]",
      "Fijian" = "982. [SEE ELECTION STUDY NOTES]",
      "Nepali" = "983. [SEE ELECTION STUDY NOTES]",
      "Samoan" = "984. [SEE ELECTION STUDY NOTES]"
    ),
    "Switzerland|2019" = list(
      "Romansh (Switzerland)" = "980. [SEE ELECTION STUDY NOTES]"
    ),
    "Taiwan|2016" = list(
      "Taiwanese" = "980. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and Taiwanese" = "981. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and Hakka" = "982. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and other Chinese dialect" = "983. [SEE ELECTION STUDY NOTES]",
      "Both Taiwanese and Hakka" = "984. [SEE ELECTION STUDY NOTES]",
      "Aboriginal language" = "985. [SEE ELECTION STUDY NOTES]",
      "Other Chinese dialect" = "986. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and Aboriginal language" = "987. [SEE ELECTION STUDY NOTES]",
      "Mandarin, Taiwanese and Hakka" = "988. [SEE ELECTION STUDY NOTES]",
      "Burmese" = "989. [SEE ELECTION STUDY NOTES]"
    ),
    "Taiwan|2020" = list(
      "Taiwanese" = "980. [SEE ELECTION STUDY NOTES]",
      "Aboriginal language" = "981. [SEE ELECTION STUDY NOTES]",      
      "Other Chinese dialect" = "982. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and Taiwanese" = "983. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and Hakka" = "984. [SEE ELECTION STUDY NOTES]",
      "Both Taiwanese and Hakka" = "985. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and other Chinese dialect" = "986. [SEE ELECTION STUDY NOTES]",
      "Mandarin, Taiwanese and English" = "987. [SEE ELECTION STUDY NOTES]",
      "Both Mandarin and English" = "988. [SEE ELECTION STUDY NOTES]",
      "Mandarin, Taiwanese and Hakka" = "989. [SEE ELECTION STUDY NOTES]"
    ),
    "Turkey|2018" = list(
      "Uzbek" = "980. [SEE ELECTION STUDY NOTES]",
      "Turkmen" = "981. [SEE ELECTION STUDY NOTES]"
    )
  ),
  Religion = list(
    "Australia|2019" = list(
      "Uniting Church in Australia" = "1420. [SEE ELECTION STUDY NOTES]"
    ),
    "Brazil|2018" = list(
      "Mormon Church, Jehovah Witness" = "1599. [SEE ELECTION STUDY NOTES]",
      "Candombla (African religion) Umbanda" = "7100. [SEE ELECTION STUDY NOTES]",
      "Espiritualism" = "7200. [SEE ELECTION STUDY NOTES]",
      "Seisho-No-Ie, World Messianic Church, Perfect Liberty" = "7900. [SEE ELECTION STUDY NOTES]",
      "Santo Daime, Esoteric" = "7901. [SEE ELECTION STUDY NOTES]"
    ),
    "Germany|2017" = list(
      "Evangelical Church in Germany (excluding Free Churches)" = "1298. [SEE ELECTION STUDY NOTES]",
      "Protestant Free Church" = "1299. [SEE ELECTION STUDY NOTES]"
    ),
    "United Kingdom|2017" = list(
      "Other - Protestant" = "1298. [SEE ELECTION STUDY NOTES]"
    ),
    "Ireland|2016" = list(
      "Quaker" = "1298. [SEE ELECTION STUDY NOTES]",
      "Humanist" = "9001. [SEE ELECTION STUDY NOTES]",
      "Jedi Knight" = "9002. [SEE ELECTION STUDY NOTES]",
      "Pastafarianism" = "9003. [SEE ELECTION STUDY NOTES]"
    ),
    "Italy|2018" = list(
      "Asian Religion" = "9001. [SEE ELECTION STUDY NOTES]"
    ),
    "Montenegro|2016" = list(
      "Montenegrin Orthodox" = "1698. [SEE ELECTION STUDY NOTES]",
      "Serbian Orthodox" = "1699. [SEE ELECTION STUDY NOTES]"
    ),
    "South Korea|2016" = list(
      "Catholic (not further specified)" =  "1199. [SEE ELECTION STUDY NOTES]"
    ),
    "Sweden|2018" = list(
      "Catholic, not further specified" = "1199. [SEE ELECTION STUDY NOTES]",
      "Orthodox, not further specified" = "1699. [SEE ELECTION STUDY NOTES]"
    ),
    "Taiwan|2016" = list(
      "Buddhist and Taoist" = "9001. [SEE ELECTION STUDY NOTES]",
      "Confucianism, Buddhism and Taoism" = "9002. [SEE ELECTION STUDY NOTES]"
    ),
    "Taiwan|2020" = list(
      "Buddhist and Taoist" = "9001. [SEE ELECTION STUDY NOTES]",
      "Falun Gong" = "9002. [SEE ELECTION STUDY NOTES]",
      "Buddhism and Christianity" = "9003. [SEE ELECTION STUDY NOTES]",
      "Nine Lotus Sacred Path" = "9004. [SEE ELECTION STUDY NOTES]"
    ),
    "Turkey|2018" = list(
      "Zoroastrian" = "9001. [SEE ELECTION STUDY NOTES]"
    ),
    "United States|2016" = list(
      "Christian, not further specified, inter- or non-denominational" = "1001. [SEE ELECTION STUDY NOTES]",
      "Evangelical Churches (Evangelical Covenant Church, Evangelical Free Church, EFC, or EFCA)" = "1298. [SEE ELECTION STUDY NOTES]",
      "Quakers; Friends" = "1299. [SEE ELECTION STUDY NOTES]",
      "Syrian Orthodox, Armenian Orthodox" = "1699. [SEE ELECTION STUDY NOTES]",
      "American Indian religions; Native American religions" = "7900. [SEE ELECTION STUDY NOTES]",
      "Catholic and Protestant" = "9001. [SEE ELECTION STUDY NOTES]",
      "Messianic Judaism; Jews for Jesus" = "9002. [SEE ELECTION STUDY NOTES]",
      "More than one major religion (e.g., Christian, Jewish, Muslim, etc.)" = "9003. [SEE ELECTION STUDY NOTES]",
      "Religious Science; Science of Mind (not Scientology; not Christian Scientists); Centers for Spiritual Living" = "9004. [SEE ELECTION STUDY NOTES]",
      "Unity; Unity Church; Christ Church Unity" = "9005. [SEE ELECTION STUDY NOTES]"
    )
    
  )
)

do.country.recodes <- function(data) {
  for(var in names(country.recodes)) {
    for(id in names(country.recodes[[var]])) {
      res <- str_split(id, "\\|", simplify = T)
      country <- res[[1]]
      year <- res[[2]]
      
      recodes <- country.recodes[[var]][[id]]
      
      for (new.cat in names(recodes)) {
        old.cat <- recodes[[new.cat]]
        
        data <- data %>% 
          mutate(across(all_of(var), ~fct_expand(.x, new.cat))) %>% 
          mutate(across(all_of(var), ~replace(.x, Country == country & Year == year & .x == old.cat, new.cat)))
      }
    }
  }
  
  data
}
