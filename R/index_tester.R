# Simple app to test difference scenarios for the ATT-Pol (Gallagher) and other
#  index calculations
launchIndexTester <- function() {
  ui <- fluidPage(
  
      # Application title
      titlePanel("Index Tester"),
  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
              textInput("group.count", "Number of Groups",  value = 2, width = "80px"),
              textInput("party.count", "Number of Parties", value = 2, width = "80px"),
              width = 2
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
              htmlOutput("gallTable"),
              h3("Indices: "),
              tableOutput("gall.res")
          )
      )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
      output$gallTable <- renderUI({
          group.headers <- map(1:input$group.count, ~tags$td(paste("Group", .x)))
          
          rows <- map(1:input$party.count, function(p) {
              cells <- map(1:input$group.count, ~ tags$td(textInput(paste0("p", p, "g", .x), "", "0", width = "80px")) )
              
              tags$tr(
                  tags$td(paste("Party", p)),
                  cells
              )
          })
          
          tags$table(
              tags$tr(
                  tags$td(""),
                  group.headers
              ),
              rows
          )
      })
      
      party.by.grp.size.dat <- reactive({
          map_dfr(c(1:input$party.count), function(p) {
              map_dfr(c(1:input$group.count), function(g) {
                  tibble(
                      Party = paste("Party", p),
                      Group = paste("Group", g),
                      n = as.numeric(input[[paste0("p", p, "g", g)]])
                  )
              })
          })
      })
  
      
      output$gall.res <- renderTable(calc.summary.indices(party.by.grp.size.dat(), include.extra = T))
  
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
