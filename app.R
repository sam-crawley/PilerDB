library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)

res <- read_rds("output/divided.rds")
category.sum <- read_rds("output/divided.category.summary.rds")
summary.table <- calc.summary.data(res)
group.sizes <- get.group.size.summary(res)
max.parties <- length(names(group.sizes)[str_detect(names(group.sizes), "^Party.Grp")])

source("gen_crosstabs.R")

ui <- navbarPage(title = "Divided Society Data",
  tabPanel("Crosstabs",
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
  ),
  tabPanel("Category Data", 
     pageWithSidebar(
       headerPanel('Category Data'),
       sidebarPanel(
         pickerInput("cat.datasrc", 
                     label = "Data source", 
                     names(category.sum)
         ),
         pickerInput("cat.var", 
                     label = "Variable", 
                     main.vars
         ),
         width = 2
       ),
       mainPanel(
         p("Question: ", textOutput("catSumQuestion")),
         DTOutput("catSumTable", height = "auto"),
         width = 10
       )
     )
  )
)

get.summary.table <- function(datasrc, country, with.id = F) {
  tab <- summary.table  %>%
    mutate(across(ends_with('.pct'), ~round(.x, digits = 2) * 100)) %>%
    rename(
      "Correlation (all)" = cor,
      "Correlation (others/missing removed)" = cor.nomiss,
      "Total Included (N)" = total.included,
      "Total Included (%)" = total.included.pct,
      "Party Missing (N)" = party.missing,
      "Party Missing (%)" = party.missing.pct,
      "Group Missing (N)" = group.missing,
      "Group Missing (%)" = group.missing.pct
    )
  
  if (! with.id)
    tab <- tab %>% select(-ID)
  
  if (! is.null(datasrc))
    tab <- tab %>%
      filter(`Data Source` %in% datasrc)
  
  if (! is.null(country))
    tab <- tab %>%
      filter(Country %in% country)
  
  tab
}

get.group.sizes <- function() {
  summary.table %>%
    select(ID, `Data Source`, Year, cor.nomiss) %>%
    inner_join(group.sizes, by = "ID") %>%
    select(Country, everything()) %>%
    select(-ID)
}

gen.group.size.names <- function(max.parties) {
  c(
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
  )
}

get.cat.sum.table <- function(data.src, var.name) {
  category.sum[[data.src]][[var.name]]$categories
}

get.cat.sum.question <- function(data.src, var.name) {
  category.sum[[data.src]][[var.name]]$question
}

server <- function(input, output, session) {
  
  output$tableOutput = renderDT(
    get.summary.table(input$datasrc, input$country), 
    options = list(
      lengthChange = F, 
      paging = F, 
      searching = F,
      buttons = c('copy', 'csv', 'excel'),
      dom = 'Bfrtip',
      order = list(list(7, 'desc'))
    ),
    server = T, 
    selection = 'single',
    class = "display compact",
    extensions = 'Buttons'
  )
  
  output$catSumTable = renderDT(
    get.cat.sum.table(input$cat.datasrc, input$cat.var),
    options = list(
      paging = F
    ),
  )
  
  output$catSumQuestion <- renderText(get.cat.sum.question(input$cat.datasrc, input$cat.var))
  
  sketch = htmltools::withTags(table(
    class = 'display compact',
    thead(
      tr(
        lapply(c("Country", "Data Source", "Year", "Correlation (missing/other removed)"), function (x) { th(rowspan = 2, x) }),
        lapply(1:summary.group.size, function (x) { th(colspan = 2, paste("Group", x)) }),
        lapply(paste("Supporters of party", 1:max.parties), function (x) { th(colspan = summary.group.size+2, x) })
      ),
      tr(
        lapply(gen.group.size.names(max.parties), th)
      )
    )
  ))
  
  output$tableGroupSizes <- renderDT(
    get.group.sizes(),
    options = list(
      lengthChange = F, 
      paging = F, 
      searching = F,
      buttons = c('copy', 'csv', 'excel'),
      dom = 'Bfrtip'
    ),
    container = sketch,
    rownames = F,
    extensions = 'Buttons'
  )
  
  observeEvent(input$tableOutput_rows_selected, {
    row <- input$tableOutput_rows_selected
    
    displayed.sum.table <- get.summary.table(input$datasrc, input$country, with.id = T)
    selected.row <- displayed.sum.table[row,]
    
    country.data <- res[[selected.row$ID]]
    
    if (is.null(session$userData$countryTabsOpen))
      session$userData$countryTabsOpen <- list()
    
    countryTabsOpen <- session$userData$countryTabsOpen
    countryTabID <- str_remove_all(selected.row$ID, " ")
    
    if (has_name(countryTabsOpen, countryTabID)) {
      updateTabsetPanel(session, "mainPanel", selected = selected.row$ID)
      return()
    }
    
    session$userData$countryTabsOpen[[countryTabID]] <- 1
    
    tab <- tabPanel(country.data$Summary$general$ID,
      h3(textOutput(paste0("CountryName", countryTabID))),
    
      h4(textOutput(paste0("LanguageHeading", countryTabID))),
      tableOutput(paste0("LanguageTable", countryTabID)),
      
      h4(textOutput(paste0("ReligionHeading", countryTabID))),
      tableOutput(paste0("ReligionTable", countryTabID)),
      
      h4(textOutput(paste0("EthnicityHeading", countryTabID))),
      tableOutput(paste0("EthnicityTable", countryTabID)),
      
      h4("Statistics"),
      textOutput(paste0("SampleSize", countryTabID)),
      h5("Correlations - all categories"),
      tableOutput(paste0("CorTable", countryTabID)),
      h5("Correlations - Missing/Other removed"),
      tableOutput(paste0("CorNoMissTable", countryTabID))
    )
    
    output[[paste0("CountryName", countryTabID)]] <- renderText(country.data$Summary$general$Country)
    output[[paste0("SampleSize", countryTabID)]] <- renderText({
      paste("Sample Size:", country.data$Summary$general$`Sample Size`)
    })
    output[[paste0("CorTable", countryTabID)]] <- renderTable(country.data$Summary$cor %>% select(-max.col))
    output[[paste0("CorNoMissTable", countryTabID)]] <- renderTable(country.data$Summary$cor.nomiss %>% select(-max.col))
    
    walk (group.names, function(group) {
      grp.output.header <- paste0(group, "Heading", countryTabID)
      grp.output.table <- paste0(group, "Table", countryTabID)
      
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