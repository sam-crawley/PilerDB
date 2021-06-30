library(tidyverse)
library(shiny)
library(DT)

res <- read_rds("output/divided.rds")

source("gen_crosstabs.R")

ui <- fluidPage(
  tabsetPanel(id = "mainPanel", 
    tabPanel("Summary",DTOutput("tableOutput", height = "auto"),)
  )
)

server <- function(input, output, session) {
  summary.table <- calc.summary.data(res) %>%
    mutate(across(ends_with('.pct'), ~round(.x, digits = 2) * 100)) %>%
    rename(
      "Group Basis" = group.basis,
      "Correlation" = cor,
      "Correlation (missing removed)" = cor.nomiss,
      "Total Included (N)" = total.included,
      "Total Included (%)" = total.included.pct,
      "Party Missing (N)" = party.missing,
      "Party Missing (%)" = party.missing.pct,
      "Group Missing (N)" = group.missing,
      "Group Missing (%)" = group.missing.pct
    )
  
  
  output$tableOutput = renderDT(
    summary.table, options = list(lengthChange = FALSE, paging = F), server = T, selection = 'single'
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