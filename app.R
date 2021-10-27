library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinybusy)

source("gen_crosstabs.R")

res <- read_rds("output/divided.rds")
crosstabs <- res$crosstabs
category.sum <- res$cat.sum
data.src.info <- res$data.src.info
summary.table <- res$summary
group.sizes <- res$group.sizes
max.parties <- res$max.parties

ui <- navbarPage(title = "Divided Society Data",
  tabPanel("Crosstabs",
    add_busy_spinner(spin = "cube-grid"),
    tabsetPanel(id = "mainPanel", 
      tabPanel("Summary",
        pageWithSidebar(
          headerPanel('Summary'),
          sidebarPanel(
            h3("Filters"),
            pickerInput("datasrc", 
                        label = "Data source", 
                        sort(unique(summary.table$`Data Source`)), 
                        multiple = T,
                        options = list(
                          `none-selected-text` = "All"
                        )),
            pickerInput("country", 
                        label = "Country", 
                        sort(unique(summary.table$`Country`)), 
                        multiple = T,
                        options = list(
                          `none-selected-text` = "All"
                        )),
            checkboxInput("incomplete.data",
                          label = "Show countries with incomplete data"
            ),
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
  tabPanel("Data Sources", 
     pageWithSidebar(
       headerPanel('Data Sources'),
       sidebarPanel(
         pickerInput("info.datasrc", 
                     label = "Data source", 
                     sort(names(data.src.info))
         ),
         width = 2
       ),
       mainPanel(
         h3("Countries"),
         textOutput("info.cntry.included"),
         h4("Countries excluded due to no (usable) party data"),
         textOutput("info.cntry.excluded.no_party"),
         h4("Countries excluded due to no (usable) group data"),
         textOutput("info.cntry.excluded.no_group"),
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
         DTOutput("catSumTable", height = "auto"),
         width = 10
       )
     )
  ),
  tabPanel("Download Data",
     fluidPage(
       headerPanel('Download Data'),
       mainPanel(
         use_busy_spinner(spin = "fading-circle", position = 'full-page'),
         p("Download data as an Excel file."),
         downloadBttn("downloadData", label = "Download"),
         width = 10
       )
     )          
  )
)

get.summary.table <- function(datasrc, country, incomplete.data = F, with.id = F) {
  tab <- summary.table  %>%
    mutate(across(ends_with('.pct'), ~round(.x, digits = 2) * 100)) %>%
    rename(
      "Correlation (full sample)" = cor,
      "Correlation (others / missing removed)" = cor.nomiss,
      "Total Included (N)" = total.included,
      "Total Included (%)" = total.included.pct,
      "Party Missing / Other (N)" = party.missing,
      "Party Missing / Other (%)" = party.missing.pct,
      "Group Missing / Other (N)" = group.missing,
      "Group Missing / Other (%)" = group.missing.pct,
      "Lng" = Language,
      "Rel" = Religion,
      "Eth" = Ethnicity
    ) %>%
    mutate(across(c(Lng, Rel, Eth), ~if_else(.x, "\u{2713}", "\u{2716}")))
  
  if (! with.id)
    tab <- tab %>% select(-ID)
  
  if (! is.null(datasrc))
    tab <- tab %>%
      filter(`Data Source` %in% datasrc)
  
  if (! is.null(country))
    tab <- tab %>%
      filter(Country %in% country)
  
  if (! incomplete.data)
    tab <- tab %>%
      filter(! is.na(`Group Basis`))
  
  tab
}

gen.group.size.names <- function(max.parties) {
  c(
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
  )
}

get.data.src.question <- function(data.src, var.name) {
  q <- data.src.info[[data.src]][['questions']][[var.name]]
  
  if (is.null(q))
    return("N/A")
  
  q
}

get.cat.sum.table <- function(category.sum, data.src, variable) {
  srcs <- names(category.sum)
  if (! is.null(data.src))
    srcs <- data.src
  
  res <- map_dfr(srcs, function(data.src) {
    bind_rows(category.sum[[data.src]], .id = "Variable") %>%
      mutate("Data Source" = data.src) %>%
      select(`Data Source`, everything())
  }) %>%
    filter(N != 0)
  
  if (! is.null(variable))
    res <- res %>% filter(Variable %in% variable)
  
  res
}

get.country.list <- function(data.src, var.name) {
  res <- paste(data.src.info[[data.src]]$countries[[var.name]], collapse = ", ")
  
  if (str_length(res) == 0)
    return ("None")
  
  res
}


server <- function(input, output, session) {
  
  output$tableOutput = renderDT(
    get.summary.table(input$datasrc, input$country, input$incomplete.data),
    options = list(
      lengthChange = F, 
      paging = F, 
      searching = F,
      order = list(list(8, 'desc')),
      columnDefs = list(list(className = 'dt-center', targets = 9:11))
    ),
    server = T, 
    selection = 'single',
    class = "display compact",
    rownames = F
  )
  
  output$catSumTable = renderDT(
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
  
  output$info.cntry.included <- renderText(get.country.list(input$info.datasrc, "included"))
  output$info.cntry.excluded.no_party <- renderText(get.country.list(input$info.datasrc, "no_party"))
  output$info.cntry.excluded.no_group <- renderText(get.country.list(input$info.datasrc, "no_group"))
  output$info.cntry.excluded.low_n <- renderText(get.country.list(input$info.datasrc, "low_n"))
  
  output$info.var.party <- renderText(get.data.src.question(input$info.datasrc, "Party"))
  output$info.var.religion <- renderText(get.data.src.question(input$info.datasrc, "Religion"))
  output$info.var.language <- renderText(get.data.src.question(input$info.datasrc, "Language"))
  output$info.var.ethnicity <- renderText(get.data.src.question(input$info.datasrc, "Ethnicity"))
  
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
    group.sizes,
    options = list(
      lengthChange = F, 
      paging = F, 
      searching = F,
      order = list(list(3, 'desc'))
    ),
    container = sketch,
    rownames = F
  )
  
  output$downloadData <- downloadHandler(
    filename <- function() {
      "divided_crosstabs.xlsx"
    },
    
    content <- function(file) {
      file.copy('output/divided_crosstabs.xlsx', file)
    }
  )
  
  observeEvent(input$tableOutput_rows_selected, {
    row <- input$tableOutput_rows_selected
    
    displayed.sum.table <- get.summary.table(input$datasrc, input$country, input$incomplete.data, with.id = T)
    selected.row <- displayed.sum.table[row,]
    
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
      h4("Group basis: ", textOutput(paste0("GroupBasis", countryTabID), inline = T)),
      
      br(),
    
      h4(textOutput(paste0("LanguageHeading", countryTabID))),
      DTOutput(paste0("LanguageTable", countryTabID)),
      
      h4(textOutput(paste0("ReligionHeading", countryTabID))),
      DTOutput(paste0("ReligionTable", countryTabID)),
      
      h4(textOutput(paste0("EthnicityHeading", countryTabID))),
      DTOutput(paste0("EthnicityTable", countryTabID)),
      
      h4("Statistics"),
      textOutput(paste0("SampleSize", countryTabID)),
      h5("Correlations - all categories"),
      tableOutput(paste0("CorTable", countryTabID)),
      h5("Correlations - all categories, weighted"),
      tableOutput(paste0("CorWtTable", countryTabID)),
      h5("Correlations - Missing/Other removed"),
      tableOutput(paste0("CorNoMissTable", countryTabID)),
      h5("Correlations - Missing/Other removed, weighted"),
      tableOutput(paste0("CorNoMissWtTable", countryTabID)),
      h5("Original country name (from data file)"),
      textOutput(paste0("country.orig", countryTabID))
    )
    
    sample.size <- country.data$Summary$general$`Sample Size`
    
    output[[paste0("CountryName", countryTabID)]] <- renderText(country.data$Summary$general$Country)
    output[[paste0("GroupBasis", countryTabID)]] <- renderText(country.data$Summary$general$`Group Basis`)
    
    output[[paste0("SampleSize", countryTabID)]] <- renderText({
      paste("Sample Size:", sample.size)
    })
    output[[paste0("CorTable", countryTabID)]] <- renderTable(country.data$Summary$cor)
    output[[paste0("CorWtTable", countryTabID)]] <- renderTable(country.data$Summary$cor.wt)
    output[[paste0("CorNoMissTable", countryTabID)]] <- renderTable(country.data$Summary$cor.nomiss)
    output[[paste0("CorNoMissWtTable", countryTabID)]] <- renderTable(country.data$Summary$cor.nomiss.wt)
    output[[paste0("country.orig", countryTabID)]] <- renderText(country.data$Summary$country.orig)
    
    walk (group.names, function(group) {
      grp.output.header <- paste0(group, "Heading", countryTabID)
      grp.output.table <- paste0(group, "Table", countryTabID)
      
      if (! is.null(country.data[[group]])) {
        output[[grp.output.header]] <- renderText(group)
        
        group.cols <- colnames(country.data[[group]])[2:ncol(country.data[[group]])]
        
        crosstab <- country.data[[group]] %>% 
          adorn_percentages("row") %>% 
          adorn_pct_formatting()
        
        group.cols.order <- unlist( map(group.cols, ~paste0(.x, c(".n", '.%'))))
        
        crosstab <- inner_join(crosstab, attr(crosstab, "core"), by = "Party", suffix = c(".%", ".n")) %>%
          select( Party, all_of(group.cols.order) ) %>%
          adorn_totals("col") %>%
          mutate(pct = paste0(round(Total / sample.size, 2)*100, '%'))
        
        col.totals.n <- country.data[[group]] %>% select(-Party) %>% summarise(across(everything(), ~sum(.x)))
        col.totals <- country.data[[group]] %>% select(-Party) %>% 
          summarise(across(everything(), ~paste0(round(sum(.x)/sample.size*100, 1), '%'))) %>% 
          bind_cols(col.totals.n, .name_repair = "unique") %>% 
          set_names(c(paste0(names(col.totals.n), '.%'), paste0(names(col.totals.n), '.n'))) %>%
          mutate(Party = "Total", Total = sample.size, pct = "") %>%
          select( Party, all_of(group.cols.order), Total, pct )
        
        sketch = htmltools::withTags(table(
          class = 'display compact',
          style = 'white-space: nowrap',
          thead(
            tr(
              th("Party", rowspan = 2),
              lapply(group.cols, function (x) { th(colspan = 2, x) }),
              th("Total", colspan = 2)
            ),
            tr(
              lapply(rep(c('N', '%'), length(group.cols)+1), th)
            )
          ),
          tfoot(
            tr(
              lapply(col.totals, th)
            )
          )
        ))        
        
        output[[grp.output.table]] <- renderDT(
          crosstab,
          options = list(
            lengthChange = F, 
            paging = F, 
            searching = F,
            bInfo = F,
            scrollX = T
          ),
          rownames = F,
          container = sketch
        )
      }
      else {
        output[[grp.output.header]] <- renderText("")
        output[[grp.output.table]]<- renderDT(NULL)
      }
    })
    
    appendTab("mainPanel", tab, select = T)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)