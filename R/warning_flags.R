# This adds "warning flags" to individual surveys if we suspect
#  there are problems with the data

# Add the warning flags to the main crosstabs structure
add.warning.flags <- function(crosstabs) {
  hgm <- get.high.group.missing(crosstabs)
  outliers <- get.outlier.cases(crosstabs)
  
  map(names(crosstabs), function (country) {
    warning.flags <- c()
    warning.flags.details <- NA

    # Add "high group missing" flag    
    hgm.country <- hgm %>% filter(datasource == country)
    
    if (nrow(hgm.country) >= 1) {
      warning.flags <- append(warning.flags, "high_group_missing")
      warning.flags.details <- list(hgm.country)
    }
    
    # Add "outlier" flag
    outlier.country <- outliers %>% filter(ID == country)
    
    if (nrow(outlier.country) >= 1) {
      warning.flags <- append(warning.flags, "outlier")
    }
    
    crosstabs[[country]]$Summary$general$warning.flags <- if_else(is_empty(warning.flags), NA_character_, paste(warning.flags, collapse = "|"))
    crosstabs[[country]]$Summary$general$warning.flags.details <- warning.flags.details
    
    crosstabs[[country]]
  }) %>% set_names(names(crosstabs))
}

# Generate a text version of the warning message
gen.warning.message <- function(warning.type, warning.details) {
  flags <- str_split(warning.type, "\\|", simplify = T)
  warnings <- c()
  
  if ("high_group_missing" %in% flags) {
    det <- warning.details[[1]][1,]
    
    warnings <- append(warnings, str_glue("High % of missing party responses for group: {det$group.name} [{det$group}]. ",
      "Missing: {det$missing * 100}%; Comparison group: {det$comp.group}; Comparison group missing: {det$comp.missing * 100}%; Missing diff: {det$missing.diff*100}"
    ))
  }
  
  if ("outlier" %in% flags) {
    warnings <- append(warnings, "The PES score for this survey is a statistical outlier for this country.")
  }
  
  return (warnings)
}


# Find cases where a group has a particularly high number of missing
#  responses for the party variable.
# We determine this by:
# 1. Look only at the 'Group Basis' (i.e. highest tau) group
# 2. Calculate the % size of the group, and % of missing on party
# 3. For each group, compare their missing % to
#     the missing % of the largest group, or if it is the
#     largest, the 2nd largest group. If missing % difference is
#     is larger than 20%, the survey is "flagged"
# 4. Remove groups smaller than 10% or less than 5% missing
get.high.group.missing <- function(crosstabs, flagged.only = T) {
  res <- map_dfr(names(crosstabs), function (country) {
    summary = crosstabs[[country]]$Summary$general
    
    if (is.na(summary$`Group Basis`))
      return (NULL)
    
    group <- summary$`Group Basis`
    
    summary.data <- crosstabs[[country]][[group]]
    
    summary.data <- config.summary.data(summary.data, drop.cats = F, weighted = T)
    
    if (! is.data.frame(summary.data))
      return (NULL)
    
    sample.size <- sum(summary.data$n)
    
    high.missing <- summary.data %>% 
      filter(! Group %in% cats.to.drop) %>%
      group_by(Group) %>% 
      mutate(missing.percent = n / sum(n), total.percent = sum(n) / sample.size) %>% 
      filter(Party == "Missing") %>%
      ungroup()
    
    if (nrow(high.missing) <= 1)
      return (NULL)
    
    mp <- high.missing %>% arrange(desc(total.percent))
    top.group <- mp$Group[[1]]
    
    high.missing <- high.missing %>%
            mutate(comp.group = if_else(Group == top.group, mp$Group[[2]], top.group)) %>%
            mutate(max.percent = if_else(Group == top.group, mp$missing.percent[[2]], mp$missing.percent[[1]])) %>%
            mutate(missing.diff = missing.percent - max.percent) %>%
            mutate(flag = if_else(missing.diff >= 0.20, T, F)) %>%
            filter(total.percent >= 0.10 & missing.percent >= 0.5)
    
    if (flagged.only)
      high.missing <- high.missing %>% filter(flag == T)
    
    return(
      high.missing %>% 
        mutate(datasource = country, group = group) %>%
        rename(
          missing = missing.percent,
          group.name = Group,
          group.size = total.percent,
          comp.missing = max.percent
        ) %>%
        select(
          datasource,
          group,
          group.name,
          group.size,
          missing,
          comp.group,
          comp.missing,
          missing.diff,
          flag
        )
    )
  })

  res %>% mutate(across(c(group.size, missing, comp.missing, missing.diff), ~round(.x, 3))) %>%
    arrange(desc(missing))
}

# Find surveys which are statistical outliers, using a hampel filter
#  Calculations are based on our PES measure for each country.
#  Any surveys for a country that are outside the hampel filter range
#   are considered outliers. However, we only check countries where
#   the PES std deviation is >= 9 (by default), to avoid
#   removing cases when there is not much variation in the PES
#   scores for the country.
get.outlier.cases <- function(crosstabs, sd.threshold = 9) {
  gal.df <- map_df(names(crosstabs), ~crosstabs[[.x]]$Summary$general %>% select(ID, Country, PES))
  
  ranges <- gal.df %>% 
    group_by(Country) %>% 
    summarise(median = median(PES, na.rm = T), mad = mad(PES, constant = 1, na.rm = T), sd = sd(PES, na.rm = T)) %>% 
    filter(sd >= sd.threshold) %>%
    mutate(min = median - 3 * mad, max = median + 3 * mad)
  
  flagged <- inner_join(gal.df, ranges, by = "Country") %>% 
    mutate(outside.range = PES < min | PES > max)
  
  return (flagged %>% filter(outside.range))
  
}