library(tidyverse)
library(shiny)
library(DT)

res <- read_rds("output/divided.rds")

source("gen_crosstabs.R")

ui <- fluidPage(
  tabsetPanel(id = "mainPanel", 
    tabPanel("Summary",DTOutput("tableOutput", height = "auto"),),
    tabPanel("Country Details",
      h3(textOutput("CountryName")),
      
      h4(textOutput("LanguageHeading")),
      tableOutput("LanguageTable"),
      
      h4(textOutput("ReligionHeading")),
      tableOutput("ReligionTable"),
      
      h4(textOutput("EthnicityHeading")),
      tableOutput("EthnicityTable")
    )
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
    
    output$CountryName <- renderText(country.data$Summary$general$Country)
    
    for (group.o in group.names) {
      local({
        group <- group.o
        
        grp.output.header <- paste0(group, "Heading")
        grp.output.table <- paste0(group, "Table")
        
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
    }
    
    updateTabsetPanel(session, "mainPanel", selected = "Country Details")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)