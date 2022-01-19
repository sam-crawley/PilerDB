data.spec <- list(
  file.name = "Divided/datasets/latino/2018/Latinobarometro_2018_Esp_Stata_v20190303.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "Which party would you vote for if elections were next Sunday?",
    "Religion" = "What is your religion?",
    "Language" = "What is your native language?",
    "Ethnicity" = "What ethnicity or race you identify best with?"
  ),
  country.format = 'spanish',
  country.dict = country.dict.es,
  field.def = c(
    "Party" = "P21STGBS.A",
    "Religion" = "S5",
    "Language" = "S13A",
    "Ethnicity" = "S6",
    "Country" = "IDENPA",
    "Year" = "NUMINVES",
    "Weight" = "WT"
  ),
  pre_fixups = function(data) {
    data %>% mutate(
      IDENPA = str_remove(as.character(haven::as_factor(IDENPA)), "\\[.+\\] ")
    )
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No responde", "No sabe", "Vota nulo/Blanco", "No vota/Ninguno", "No inscrito/No tenia edad"),
    "Other" = c("Otros (partidos nacionales y provinciales)")
  ),
  Language = list(
    "Other" = c("Otra")
  ),
  Religion = list(
    "Missing" = c("No contesta", "No sabe"),
    "Other" = c("Otros"),
    "No Religion" = c("Creyente, no pertenece a Iglesia", "Agnostico", "Ateo", "Ninguno")
  ),
  Ethnicity = list(
    "Missing" = c("No contesta", "No sabe"),
    "Other" = c("Otra raza")
  )
)