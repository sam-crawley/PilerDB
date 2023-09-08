#' Index tester shiny app
#'
#' Simple app to test difference scenarios for the PES and other
#'  index calculations

calc.test.indicies <- function(data) {
  if (nrow(data) == 0 | all(data$n == 0))
    return (data.frame())
  
  res <- calc.summary.indices(data, include.extra = T)
  
  extra <- map_dfr(1:nrow(data), function(row.n) {
    row <- data[row.n,]
    
    if (row$n == 0)
      return (NULL)
    
    tibble(
      Party = rep(row$Party, row$n),
      Group = rep(row$Group, row$n)
    )
  })
  
  if (nrow(unique(extra)) == 1)
    return (data.frame())
  
  assoc.res <- StatMatch::pw.assoc(Group ~ Party, extra, out.df = T)
  
  res$V <- assoc.res$V
  res$tau <- assoc.res$tau
  
  res
}

#' @export
launchIndexTester <- function() {
  ui <- fluidPage(
  
      # Application title
      titlePanel("Index Tester"),
  
      sidebarLayout(
          sidebarPanel(
              textInput("group.count", "Number of Groups",  value = 2, width = "80px"),
              textInput("party.count", "Number of Parties", value = 2, width = "80px"),
              width = 2
          ),
  
          mainPanel(
              htmlOutput("gallTable"),
              h3("Indices: "),
              tableOutput("gall.res")
          )
      )
  )
  
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
  
      
      output$gall.res <- renderTable(calc.test.indicies(party.by.grp.size.dat()))
  
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
