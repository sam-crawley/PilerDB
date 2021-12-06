library(shiny)

source("util.R")
source("../gen_crosstabs.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gallagher Test"),

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
            h3("Gallagher: "),
            textOutput("gall.res"),
            tableOutput("test")
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
                    val = as.numeric(input[[paste0("p", p, "g", g)]])
                )
            })
        })
    })

    
    output$gall.res <- renderText(calc.test.gall(party.by.grp.size.dat()))

}

# Run the application 
shinyApp(ui = ui, server = server)
