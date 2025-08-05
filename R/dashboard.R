#' Dashboard shiny app to display PILER DB
#' @export
#' @import shiny
#' @import shinyWidgets
launchPilerDash <- function(logger = NULL) {
  if (is.null(logger)) {
    logger <- log4r::logger(threshold = "DEBUG")
  }
  
  log4r::info(logger, "PILER Dashboard initialising")
  
  crosstabs <- piler$crosstabs
  category.sum <- piler$cat.sum
  data.src.info <- piler$data.src.info
  summary.table <- piler$summary
  group.sizes <- piler$group.sizes
  max.parties <- piler$max.parties
  countries <- unique(piler$survey.summary$Country) %>% sort()
  party.map <- get.party.map()
  
  data.src.list <- sort(names(piler$data.src.info))
  
  excel.dir <- get.excel.dir()
  
  log4r::debug(logger, "Path to excel files: ", excel.dir)

  ui <- navbarPage(title = "PILER DB", header = tags$div(style="float: right; margin-right: 10px", tags$b("DB Version:"), piler$version),
    tabPanel("Crosstabs",
      shinybusy::add_busy_spinner(spin = "cube-grid"),
      tabsetPanel(id = "mainPanel", 
        tabPanel("Summary",
          pageWithSidebar(
            headerPanel('Summary'),
            sidebarPanel(
              h3("Filters"),
              pickerInput("datasrc", 
                          label = "Data source", 
                          data.src.list, 
                          multiple = T,
                          options = list(
                            `none-selected-text` = "All"
                          )),
              pickerInput("country", 
                          label = "Country", 
                          countries, 
                          multiple = T,
                          options = list(
                            `none-selected-text` = "All"
                          )),
              pickerInput("group.basis", 
                          label = "Group Basis", 
                          c('(Highest PES)', group.names, '(All)'),
                          multiple = F
                          ),            
              checkboxInput("incomplete.data",
                            label = "Show countries with incomplete data"
              ),
              width = 2
            ),
            mainPanel(
              DT::DTOutput("tableOutput", height = "auto"),
              width = 10
            )
          )
        ),
        tabPanel("Group Sizes",
           pickerInput("group.basis.gs", 
                       label = "Group Basis", 
                       sort(c('(Highest PES)', group.names)), 
                       multiple = F
           ),                
          DT::DTOutput("tableGroupSizes")
        )
      )
    ),
    tabPanel("Countries",
      tabsetPanel(id = "countryPanel", 
                 tabPanel("Summary",
                    DT::DTOutput("countrySummary", height = "auto")
                 ),
                 tabPanel("Country Details", pageWithSidebar(
                   headerPanel('Country Details'),
                   sidebarPanel(
                     pickerInput("country.picker", 
                                 label = "Country", 
                                 countries
                     ),
                     width = 2
                   ),
                   mainPanel(
                     DT::DTOutput("tableCountryParties")
                   )
                 ))
      )
    ),
    tabPanel("Data Sources", 
       pageWithSidebar(
         headerPanel('Data Sources'),
         sidebarPanel(
           pickerInput("info.datasrc", 
                       label = "Data source", 
                       data.src.list
           ),
           width = 2
         ),
         mainPanel(
           h3("Countries included"),
           textOutput("info.cntry.included"),
           h3("Countries excluded"),
           textOutput("info.cntry.excluded"),
           h3("Questions"),
           h4("Party"),
           textOutput("info.var.party"),
           h4("Language"),
           textOutput("info.var.language"),
           h4("Religion"),
           textOutput("info.var.religion"),
           h4("Ethnicity"),
           textOutput("info.var.ethnicity"),
           width = 10
         )
       )
    ),  
    tabPanel("Category Data", 
       pageWithSidebar(
         headerPanel('Category Data'),
         sidebarPanel(
           pickerInput("cat.datasrc", 
                       label = "Data source", 
                       sort(names(data.src.info)),
                       multiple = T,
                       options = list(
                         `none-selected-text` = "All"
                       )
           ),
           pickerInput("cat.var", 
                       label = "Variable", 
                       main.vars,
                       multiple = T,
                       options = list(
                         `none-selected-text` = "All"
                       )
           ),
           width = 2
         ),
         mainPanel(
           DT::DTOutput("catSumTable", height = "auto"),
           width = 10
         )
       )
    ),
    tabPanel("Download Data",
       fluidPage(
         headerPanel('Download Data'),
         mainPanel(
           p("Download summary data as Excel file."),
           downloadBttn("downloadSummaryData", label = "Download"),
           p("Download crosstab data as Excel file."),
           downloadBttn("downloadCrosstabData", label = "Download"),         
           width = 10
         )
       )          
    )
  )
  
  server <- function(input, output, session) {
    summary_table <- reactive({
      get.summary.table(
        piler, 
        input$datasrc, 
        input$group.basis, 
        input$country, 
        input$incomplete.data, 
        with.id = T
      )
    })
    
    output$tableOutput = DT::renderDT({
      table <- req(summary_table()) %>%
        select(-ID)
      
      table %>%
        DT::datatable(
          options = list(
            lengthChange = F, 
            paging = F, 
            searching = F,
            order = list(list(5, 'desc')),
            columnDefs = list(list(className = 'dt-center', targets = 12:14))
          ),
          selection = 'single',
          class = "display compact",
          rownames = F
        ) %>%
        DT::formatRound(c("Total Included (%)", "Party Missing / Other (%)", "Group Missing / Other (%)"), 2)
      },
      server = T
    )
    
    output$catSumTable = DT::renderDT(
      get.cat.sum.table(category.sum, input$cat.datasrc, input$cat.var),
      options = list(
        paging = T,
        pageLength = 1000,
        buttons = c('excel'),
        dom = 'Bfrtip'
      ),
      rownames = F,
      extensions = 'Buttons'
    )
    
    output$tableCountryParties = DT::renderDT(
      get.country.parties(piler, input$country.picker, party.map),
      options = list(
        paging = F, 
        searching = F
      ),
      rownames = F
    )
    
    output$info.cntry.included <- renderText(get.country.list(input$info.datasrc, included = T))
    output$info.cntry.excluded <- renderText(get.country.list(input$info.datasrc, included = F))
    
    output$info.var.party <- renderText(get.data.src.question(data.src.info, input$info.datasrc, "Party"))
    output$info.var.religion <- renderText(get.data.src.question(data.src.info, input$info.datasrc, "Religion"))
    output$info.var.language <- renderText(get.data.src.question(data.src.info, input$info.datasrc, "Language"))
    output$info.var.ethnicity <- renderText(get.data.src.question(data.src.info, input$info.datasrc, "Ethnicity"))
    
    sketch = htmltools::withTags(table(
      class = 'display compact',
      thead(
        tr(
          lapply(c("Country", "Data Source", "Year", "Group Basis"), function (x) { th(rowspan = 2, x) }),
          lapply(1:summary.group.size, function (x) { th(colspan = 2, paste("Group", x)) }),
          lapply(paste("Supporters of party", 1:max.parties), function (x) { th(colspan = summary.group.size+3, x) })
        ),
        tr(
          lapply(gen.group.size.names(max.parties), th)
        )
      )
    ))
    
    output$tableGroupSizes <- DT::renderDT(
      get.group.sizes(group.sizes, piler$group.sizes.by.group, input$group.basis.gs),
      options = list(
        lengthChange = F, 
        paging = F, 
        searching = F,
        order = list(list(3, 'desc'))
      ),
      container = sketch,
      rownames = F
    )
  
    output$downloadSummaryData <- downloadHandler(
      filename <- function() {
        "piler_summary.xlsx"
      },
      
      content <- function(file) {
        file.copy(paste0(excel.dir, '/piler_summary.xlsx'), file)
      }
    )
    
      
    output$downloadCrosstabData <- downloadHandler(
      filename <- function() {
        "piler_crosstabs.xlsx"
      },
      
      content <- function(file) {
        file.copy(paste0(excel.dir, '/piler_crosstabs.xlsx'), file)
      }
    )
    
    output$countrySummary <- DT::renderDT(
      get.country.summary(piler$country.summaries),
      options = list(
        lengthChange = F, 
        paging = F, 
        searching = T,
        order = list(list(4, 'desc'))
      ),
      rownames = F
    )
    
    observeEvent(input$tableOutput_rows_selected, {
      row <- input$tableOutput_rows_selected
      req(row)  # Ensure a row is selected
      
      selected.row <- summary_table()[row, ]
      
      country.data <- crosstabs[[selected.row$ID]]
      
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
        
        pickerInput(
          inputId = paste0("CatsToDrop", countryTabID),
          label = "Drop Categories:",
          choices = list(
            "None" = "none",
            "Missing/Other" = "mis_oth", 
            "Missing/Other/No Rel." = "mis_oth_norel"
          ),
          selected = "mis_oth_norel",
          inline = T
        ),
        
        prettySwitch(
          inputId = paste0("ShowWeighted", countryTabID),
          label = "Show weighted data",
          fill = TRUE, 
          status = "primary",
          value = T,
          inline = T
        ),
        
        actionBttn(
          inputId = paste0("Close", countryTabID),
          label = "Close",
          icon = icon("window-close"),
          size = "md",
          block = FALSE,
          no_outline = TRUE
        ),
        
        h4("Group basis: ", textOutput(paste0("GroupBasis", countryTabID), inline = T)),

        br(),
        
        h4(textOutput(paste0("WarningHeader", countryTabID))),
        htmlOutput(paste0("WarningMsg", countryTabID)),
  
        h4(textOutput(paste0("LanguageHeading", countryTabID))),
        DT::DTOutput(paste0("LanguageTable", countryTabID)),
        
        h4(textOutput(paste0("ReligionHeading", countryTabID))),
        DT::DTOutput(paste0("ReligionTable", countryTabID)),
        
        h4(textOutput(paste0("EthnicityHeading", countryTabID))),
        DT::DTOutput(paste0("EthnicityTable", countryTabID)),
        
        h4("Statistics"),
        textOutput(paste0("SampleSize", countryTabID)),
        textOutput(paste0("PartyQuestionType", countryTabID)),
        h5("Correlations - all categories"),
        tableOutput(paste0("CorTable", countryTabID)),
        h5("Correlations - all categories, weighted"),
        tableOutput(paste0("CorWtTable", countryTabID)),
        h5("Correlations - Missing/Other removed"),
        tableOutput(paste0("CorInclNoRelTable", countryTabID)),
        h5("Correlations - Missing/Other removed, weighted"),
        tableOutput(paste0("CorInclNoRelWtTable", countryTabID)),
        h5("Correlations - Missing/Other/No Rel removed"),
        tableOutput(paste0("CorAllRemTable", countryTabID)),
        h5("Correlations - Missing/Other/No Rel removed, weighted"),
        tableOutput(paste0("CorAllRemWtTable", countryTabID)),
        h5("Original country name (from data file)"),
        textOutput(paste0("country.orig", countryTabID))
      )
      
      country.warnings <- get.country.warnings(country.data)
      if (country.warnings$has.warning) {
        output[[paste0("WarningHeader", countryTabID)]] <- renderText("Warnings")
        output[[paste0("WarningMsg", countryTabID)]] <- renderUI(HTML(paste(country.warnings$message, collapse = "<br/><br/>")))
      }
      
      observeEvent(input[[paste0("CatsToDrop", countryTabID)]], {
        generate.country.tables(countryTabID, country.data, output, 
                                cats.to.drop = input[[paste0("CatsToDrop", countryTabID)]],
                                show.weighted = input[[paste0("ShowWeighted", countryTabID)]],
                                party.map = party.map)
      }, ignoreInit = T)
      
      observeEvent(input[[paste0("ShowWeighted", countryTabID)]], {
        generate.country.tables(countryTabID, country.data, output, 
                                cats.to.drop = input[[paste0("CatsToDrop", countryTabID)]],
                                show.weighted = input[[paste0("ShowWeighted", countryTabID)]],
                                party.map = party.map)
      }, ignoreInit = T)    
      
      sample.size <- country.data$Summary$general$`Sample Size`
      
      output[[paste0("CountryName", countryTabID)]] <- renderText(country.data$Summary$general$Country)
      output[[paste0("GroupBasis", countryTabID)]] <- renderText(country.data$Summary$general$`Group Basis`)
      output[[paste0("AvailCounts", countryTabID)]] <- renderTable(country.data$Summary$avail.counts %>% as_tibble())
      
      output[[paste0("SampleSize", countryTabID)]] <- renderText({
        paste("Sample Size:", sample.size)
      })
      output[[paste0("PartyQuestionType", countryTabID)]] <- renderText({
        paste("Party Question Type:", country.data$Summary$general$`Party Question Type`)
      })      
      output[[paste0("CorTable", countryTabID)]] <- renderTable(country.data$Summary$cor)
      output[[paste0("CorWtTable", countryTabID)]] <- renderTable(country.data$Summary$cor.wt)
      output[[paste0("CorInclNoRelTable", countryTabID)]] <- renderTable(country.data$Summary$cor.incl_no_rel)
      output[[paste0("CorInclNoRelWtTable", countryTabID)]] <- renderTable(country.data$Summary$cor.incl_no_rel.wt)
      output[[paste0("CorAllRemTable", countryTabID)]] <- renderTable(country.data$Summary$cor.all_removals)
      output[[paste0("CorAllRemWtTable", countryTabID)]] <- renderTable(country.data$Summary$cor.all_removals.wt)
      output[[paste0("country.orig", countryTabID)]] <- renderText(country.data$Summary$country.orig)
      
      observeEvent(input[[paste0("Close", countryTabID)]], {
        removeTab("mainPanel", country.data$Summary$general$ID)
        session$userData$countryTabsOpen[[countryTabID]] <- NULL
      }, ignoreInit = T)
      
      generate.country.tables(countryTabID, country.data, output, party.map = party.map)
      
      appendTab("mainPanel", tab, select = T)
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}