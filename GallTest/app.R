library(shiny)

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
        
        def.group.size <- round(1/as.numeric(input$group.count), 2)
        group.sizes <- map(1:input$group.count, ~tags$td(textInput(paste0("g", .x, ".size"), "Size", def.group.size, width = "80px")))
        def.party.size <- round(1/as.numeric(input$party.count), 2)
        
        rows <- map(1:input$party.count, function(p) {
            party.size <- tags$td(textInput(paste0("p", p, ".size"), "Size", def.party.size, width = "80px"))
            cells <- map(1:input$group.count, ~ tags$td(textInput(paste0("p", p, "g", .x), "", def.group.size, width = "80px")) )
            
            tags$tr(
                tags$td(paste("Party", p)),
                party.size,
                cells
            )
        })
        
        tags$table(
            tags$tr(
                tags$td(""),
                tags$td(""),
                group.headers
            ),
            tags$tr(
                tags$td(""),
                tags$td(""),
                group.sizes
            ),
            rows
        )
    })
    
    party.by.grp.size.dat <- reactive({
        map_dfr(c(1:input$party.count), function(p) {
            map_dfr(c(1:input$group.count), function(g) {
                tibble(
                    Party = paste("Party", p),
                    Group = g,
                    val = as.numeric(input[[paste0("p", p, "g", g)]])
                )
            })
        }) %>% pivot_wider(names_from = "Group", values_from = "val", names_glue = "Group {Group}")
    })
    
    party.sizes.dat <- reactive({
        map_dfr(c(1:input$party.count), function(p) {
            tibble(
                Party = paste("Party", p),
                percent = as.numeric(input[[paste0("p", p, ".size")]])
            )
        })
    })
    
    group.sizes.dat <- reactive({
        map_dfr(c(1:input$group.count), function(g) {
            tibble(
                group = paste("Group", g),
                percent = as.numeric(input[[paste0("g", g, ".size")]])
            )
        })
    })    
    
    output$gall.res <- renderText(calc.test.gall(party.by.grp.size.dat(),  group.sizes.dat(), party.sizes.dat()))

}

calc.test.gall <- function(party.by.grp, party.sizes, group.sizes) {
    gallagher.impl(party.by.grp, party.sizes, group.sizes)
}

# Run the application 
shinyApp(ui = ui, server = server)
