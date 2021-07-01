library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)

res <- read_rds("output/divided.rds")
summary.table <- calc.summary.data(res)
group.sizes <- get.group.size.summary(res)

source("gen_crosstabs.R")

ui <- fluidPage(
  tabsetPanel(id = "mainPanel", 
    tabPanel("Summary",
      pageWithSidebar(
        headerPanel('Summary'),
        sidebarPanel(
          h3("Filters"),
          pickerInput("datasrc", 
                      label = "Data source", 
                      unique(summary.table$`Data Source`), 
                      multiple = T,
                      options = list(
                        `none-selected-text` = "All"
                      )),
          pickerInput("country", 
                      label = "Country", 
                      unique(summary.table$`Country`), 
                      multiple = T,
                      options = list(
                        `none-selected-text` = "All"
                      )),          
          width = 2
        ),
        mainPanel(
          DTOutput("tableOutput", height = "auto"),
          width = 10
        )
      )
    ),
    tabPanel("Group Sizes",
      DTOutput("tableGroupSizes")
    )
  )
)

get.summary.table <- function(datasrc, country) {
  tab <- summary.table  %>%
    mutate(across(ends_with('.pct'), ~round(.x, digits = 2) * 100)) %>%
    rename(
      "Correlation (all)" = cor,
      "Total Included (N)" = total.included,
      "Total Included (%)" = total.included.pct,
      "Party Missing (N)" = party.missing,
      "Party Missing (%)" = party.missing.pct,
      "Group Missing (N)" = group.missing,
      "Group Missing (%)" = group.missing.pct
    ) %>%
    select(-cor.nomiss)
  
  if (! is.null(datasrc))
    tab <- tab %>%
      filter(`Data Source` %in% datasrc)
  
  if (! is.null(country))
    tab <- tab %>%
      filter(Country %in% country)
  
  tab
}

gen.group.size.names <- function(max.parties) {
  names <- c(
    "Country",
    #"Correlation",
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste(1:summary.group.size, "Group")), max.parties)
  )
}

server <- function(input, output, session) {
  
  output$tableOutput = renderDT(
    get.summary.table(input$datasrc, input$country), 
    options = list(lengthChange = FALSE, paging = F, searching = F),
    server = T, 
    selection = 'single',
    class = "display compact"
  )
  
  max.parties <- length(names(group.sizes)[str_detect(names(group.sizes), "^Party.Grp")])
  
  sketch = htmltools::withTags(table(
    class = 'display compact',
    thead(
      tr(
        th(colspan = 1, ' '),
        lapply(1:summary.group.size, function (x) { th(colspan = 2, paste("Group", x)) }),
        lapply(paste("Supporters of party", 1:max.parties), function (x) { th(colspan = 5, x) })
      ),
      tr(
        lapply(gen.group.size.names(max.parties), th)
      )
    )
  ))
  
  output$tableGroupSizes <- renderDT(
    group.sizes,
    options = list(lengthChange = FALSE, paging = F, searching = F),
    container = sketch,
    rownames = F
  )
  
  observeEvent(input$tableOutput_rows_selected, {
    row <- input$tableOutput_rows_selected
    
    country.data <- get.country.data(res, row)
    
    if (is.null(session$userData$tabCount))
      session$userData$tabCount <- 0
    
    session$userData$tabCount <- session$userData$tabCount + 1
    tabNum <- session$userData$tabCount
    
    tab <- tabPanel(country.data$Summary$general$ID,
      h3(textOutput(paste0("CountryName", tabNum))),
    
      h4(textOutput(paste0("LanguageHeading", tabNum))),
      tableOutput(paste0("LanguageTable", tabNum)),
      
      h4(textOutput(paste0("ReligionHeading", tabNum))),
      tableOutput(paste0("ReligionTable", tabNum)),
      
      h4(textOutput(paste0("EthnicityHeading", tabNum))),
      tableOutput(paste0("EthnicityTable", tabNum))
    )
    
    output[[paste0("CountryName", tabNum)]] <- renderText(country.data$Summary$general$Country)
    
    walk (group.names, function(group) {
      grp.output.header <- paste0(group, "Heading", tabNum)
      grp.output.table <- paste0(group, "Table", tabNum)
      
      if (! is.null(country.data[[group]])) {
        output[[grp.output.header]] <- renderText(group)
        
        crosstab <- country.data[[group]] %>% 
          adorn_percentages("row") %>% 
          adorn_pct_formatting() %>% 
          adorn_ns()
        
        output[[grp.output.table]] <- renderTable(crosstab, digits = 0)
      }
      else {
        output[[grp.output.header]] <- renderText("")
        output[[grp.output.table]]<- renderTable(tibble())
      }
    })
    
    appendTab("mainPanel", tab, select = T)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)