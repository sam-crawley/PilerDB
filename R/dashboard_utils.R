# Some functions to manipulate data for display in the shiny app

# Get the main 'summary' table on the Crosstabs tab of the shiny app
get.summary.table <- function(res, datasrc, group.basis, country, incomplete.data = F, with.id = F) {
  table.to.use <- res$summary
  if (group.basis != "(Highest PES)")
    table.to.use <- res$summary.by.group[[group.basis]]
  
  tab <- table.to.use  %>%
    mutate(across(ends_with('.pct'), ~.x * 100)) %>%
    rename(
      "PES (nrm)" = PES.nrm,
      "Tau" = cor.all_removals,
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
      "Flagged" = warning.flags,
      "CC" = cross.cutting
    ) %>%
    mutate(across(c(Lng, Rel, Eth), ~if_else(.x, "\u{2713}", "\u{2716}"))) %>%
    mutate(Flagged = if_else(is.na(Flagged), "", "\u{D83D}\u{DEA9}")) 
  
  if (group.basis %in% c("Religion", "(Highest PES)")) {
    tab <- tab %>%
      rename(
        "PES (w/ no rel.)" = PES.incl_no_rel,
        "PES (w/ no rel. nrm)" = PES.incl_no_rel.nrm
      )
  }

  tab <- tab %>%
    select(Country, `Data Source`, Year, `Sample Size`, `Group Basis`, PES, `PES (nrm)`, any_of(c("PES (w/ no rel.)", "PES (w/ no rel. nrm)")), 
            Tau, V, PVF, PVP, CC, Lng, Rel, Eth, Flagged, everything())
  
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
    rep(c("Party", "Std Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
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
      filter(`Data Source` == data.src & ! is.na(cor.all_removals))
  else
    res <- summary.table %>% 
      filter(`Data Source` == data.src & is.na(cor.all_removals))
  
  res <- paste(unique(res$Country), collapse = ", ")
  
  if (str_length(res) == 0)
    return ("None")
  
  res
}

generate.country.tables <- function(countryTabID, country.data, output, cats.to.drop = "mis_oth_norel", show.weighted = T, party.map) {
  walk (group.names, function(group) {
    grp.output.header <- paste0(group, "Heading", countryTabID)
    grp.output.table <- paste0(group, "Table", countryTabID)
    
    country <- country.data$Summary$general$Country
    country.party.map <- party.map %>% filter(Country == country)
    
    if (cats.to.drop == 'none')
      cats.to.drop <- NULL

    crosstab <- gen.crosstab(country.data[[group]], totals = T, drop.cats = cats.to.drop, weighted = show.weighted,
                             party.map = country.party.map)
    
    if (! is.null(crosstab) & is.data.frame(crosstab)) {
      if (is.null(cats.to.drop)) {
        sample.size <- country.data$Summary$general$`Sample Size`
      }
      else if (cats.to.drop == 'mis_oth') {
        sample.size <- country.data$Summary$cor.incl_no_rel %>% filter(group == group) %>% pull(n.eff)
      }
      else {
        sample.size <- country.data$Summary$cor.all_removals %>% filter(group == group) %>% pull(n.eff)
      }
      
      output[[grp.output.header]] <- renderText(group)
      
      group.names <- attr(crosstab, "group.list")
      
      crosstab <- crosstab %>%
        mutate(across(ends_with("percent"), ~paste0(format(.x, nsmall = 1), '%')))

      # Make original party name available (and link to Party Facts entry)
      crosstab <- crosstab %>%
        mutate(Party = if_else(! is.na(Party.Std), 
                               paste0("<a href='https://partyfacts.herokuapp.com/data/partycodes/", party_id, 
                                      "/' target='_new' title='Original name: ", Party, "'>", 
                                      Party.Std, "</a>"),
                               Party)) %>%
        select(-Party.Std, -party_id)

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
        escape = F,
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

get.country.summary <- function(summary) {
  summary %>% 
    group_by(Country) %>% 
    summarise(
      total.surveys = n(), 
      included = sum(is.na(excluded)), 
      excluded = sum(! is.na(excluded)), 
      pes.min = min(PES, na.rm = T), 
      pes.max = max(PES, na.rm = T),
      year.min = min(Year), 
      year.max = max(Year),
      group.basis = {
         counts <- count(cur_data(), `Group Basis`, sort = TRUE)
         if (nrow(counts) == 0) NA_character_ else counts$`Group Basis`[1]
      }
    )
}

get.country.parties <- function(piler, country, party.map) {
  surveys <- piler$summary %>%
    filter(Country == country & is.na(excluded)) %>%
    pull(ID)
  
  country.party.map <- party.map %>% filter(Country == country)
  
  map_dfr(surveys, function(s) {
    country.data <- piler$crosstabs[[s]]
    group.basis <- country.data$Summary$general$`Group Basis`
    
    if (is.na(group.basis))
      return (NULL)
    
    crosstab <- gen.crosstab(country.data[[group.basis]], weighted = T, totals = T, party.map = country.party.map) %>%
      filter(! Party %in% c(cats.to.drop, "Total")) %>%
      mutate(Party = if_else(is.na(Party.Std), Party, Party.Std))
    
    tibble(
      id = s,
      year = country.data$Summary$general$Year,
      party = crosstab$Party,
      percent = crosstab$Total_percent
    )
    
  }) %>% arrange(year) %>% 
    pivot_wider(names_from = id, id_cols = party, values_from = percent)
}

get.excel.dir <- function() {
    system.file("excel", package="PilerDB", mustWork = T)
}