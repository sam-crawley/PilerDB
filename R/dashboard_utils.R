# Some functions to manipulate data for display in the shiny app

# Get the main 'summary' table on the Crosstabs tab of the shiny app
get.summary.table <- function(res, datasrc, group.basis, country, incomplete.data = F, with.id = F) {
  table.to.use <- res$summary
  if (group.basis != "(Highest PES)")
    table.to.use <- res$summary.by.group[[group.basis]]
  
  tab <- table.to.use  %>%
    mutate(across(ends_with('.pct'), ~round(.x, digits = 2) * 100)) %>%
    rename(
      "Tau" = cor.nomiss,
      "Total Included (N)" = total.included,
      "Total Included (%)" = total.included.pct,
      "Party Missing / Other (N)" = party.missing,
      "Party Missing / Other (%)" = party.missing.pct,
      "Group Missing / Other (N)" = group.missing,
      "Group Missing / Other (%)" = group.missing.pct,
      "Lng" = Language,
      "Rel" = Religion,
      "Eth" = Ethnicity,
      "Excluded Reason" = excluded,
      "CC" = cross.cutting
    ) %>%
    mutate(across(c(Lng, Rel, Eth), ~if_else(.x, "\u{2713}", "\u{2716}"))) %>%
    select(Country, `Data Source`, Year, `Sample Size`, `Group Basis`, PES, PES.nrm, Tau, PVF, PVP, CC, Lng, Rel, Eth, everything())
  
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
    filter(! is.na(`Group Basis`)) %>% 
    select(-`Excluded Reason`)
  
  tab
}

gen.group.size.names <- function(max.parties) {
  c(
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
  )
}

get.group.sizes <- function(group.sizes, group.sizes.by.group, group.basis) {
  if (group.basis != "(Highest PES)")
    group.sizes <- group.sizes.by.group[[group.basis]]
  
  group.sizes
}

get.data.src.question <- function(data.src.info, data.src, var.name) {
  info <- data.src.info[[data.src]]
  
  if (is.null(info)) {
    # If we didn't find data source info, this must be a multiwave data set,
    #  so strip of trailing digits and try again
    info <- data.src.info[[ str_remove(data.src, "\\d+$") ]]
  }
  
  q <- info[['questions']][[var.name]]
  
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

get.country.list <- function(summary.table, data.src, included = T) {
  if (included)
    res <- summary.table %>% 
      filter(`Data Source` == data.src & ! is.na(cor.nomiss))
  else
    res <- summary.table %>% 
      filter(`Data Source` == data.src & is.na(cor.nomiss))
  
  res <- paste(unique(res$Country), collapse = ", ")
  
  if (str_length(res) == 0)
    return ("None")
  
  res
}

generate.country.tables <- function(countryTabID, country.data, output, show.all.data = T, show.weighted = F) {
  walk (group.names, function(group) {
    grp.output.header <- paste0(group, "Heading", countryTabID)
    grp.output.table <- paste0(group, "Table", countryTabID)
    
    crosstab <- gen.crosstab(country.data[[group]], totals = T, drop.cats = ! show.all.data, weighted = show.weighted)
    
    if (! is.null(crosstab) & is.data.frame(crosstab)) {
      if (show.all.data) {
        sample.size <- country.data$Summary$general$`Sample Size`
      }
      else {
        sample.size <- country.data$Summary$cor.nomiss %>% filter(group == group) %>% pull(n.eff)
      }
      
      output[[grp.output.header]] <- renderText(group)
      
      group.names <- attr(crosstab, "group.list")
      
      crosstab <- crosstab %>%
        mutate(across(ends_with("percent"), ~paste0(format(.x, nsmall = 1), '%')))
      
      col.totals <- crosstab %>% filter(Party == "Total")
      crosstab   <- crosstab %>% filter(Party != "Total")
      
      sketch = htmltools::withTags(table(
        class = 'display compact',
        style = 'white-space: nowrap',
        thead(
          tr(
            th("Party", rowspan = 2),
            lapply(group.names, function (x) { th(colspan = 2, x) }),
            th("Total", colspan = 2)
          ),
          tr(
            lapply(rep(c('N', '%'), length(group.names)+1), th)
          )
        ),
        tfoot(
          tr(
            lapply(col.totals, th)
          )
        )
      ))        
      
      output[[grp.output.table]] <- DT::renderDT(
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
      output[[grp.output.table]]<- DT::renderDT(NULL)
    }
  })
}

get.country.warnings <- function(country.data) {
  warnings <- list(has.warning = F)
  
  if (! is.na(country.data$Summary$general$warning.flags)) {
    warnings$has.warning <- T
    warnings$type <- country.data$Summary$general$warning.flags
    warnings$message <- gen.warning.message(country.data$Summary$general$warning.flags, country.data$Summary$general$warning.flags.details)
  }
  
  return (warnings)
  
}

get.excel.dir <- function() {
    system.file("excel", package="PilerDB", mustWork = T)
}