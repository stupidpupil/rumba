library(shiny)
library(tidyverse)

renderLogviewer <- function(logLinesReactive){
  return(renderUI({
    
    lines <- logLinesReactive()

    uninterestingLines <- unlist(str_split("
R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.
Natural language support but running in an English locale
R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.
Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.
Loading required package: shiny
Geospatial Data Abstraction Library extensions to R successfully loaded", "\n"))

    errorRegexp <- "^(Error|Stack trace)\\b"
    warningRegexp <- "^Warning\\b"
    positiveRegexp <- "^Listening on http://"
    uninterestingRegexp <- paste0("^\\s*(", paste(
      "Copyright",
      "Loading required package:",
      "── Attaching packages",
      "✔.+✔.+",
      "(rgeos|rgdal):? version:",
      "GEOS runtime version",
      "Linking to sp version:",
      "Loaded (PROJ\\.4|GDAL) runtime:",
      "Path to (PROJ\\.4|GDAL) shared files:",
      "Polygon checking: TRUE ",
      sep = "|"), ")")


    lines <- tibble(
      text = lines 
    ) %>%
    mutate(
      error = str_detect(text, errorRegexp),
      warning = str_detect(text, warningRegexp),
      positive = str_detect(text, positiveRegexp),
      empty = str_trim(text) == "",
      uninteresting = 
        (!error & !warning & !positive) &
        (str_detect(text, uninterestingRegexp) | (text %>% map_lgl(function(x){any(str_trim(x) == uninterestingLines)})))

    ) %>%
    mutate(
      `uninteresting-group` = uninteresting & (
        (lag(uninteresting, 2) & lag(uninteresting)) |
        (lag(uninteresting)   & lead(uninteresting)) |
        (lead(uninteresting)) & lead(uninteresting, 2)
        ),

      `uninteresting-group` = ifelse(is.na(`uninteresting-group`), FALSE, `uninteresting-group`),
      startUninterestingGroup = !lag(`uninteresting-group`) & `uninteresting-group`
    )

    retList <- list()
    retList[[1]] <- tags$tr(class="bookend",
      tags$td(`data-line-number` = " ", class="line-number"), tags$td(class="line", ""))

    possibleLineClasses <- c("uninteresting", "error", "warning", "positive", "empty", "uninteresting-group")

    for (i in 1:nrow(lines)) {
      lineClasses <- possibleLineClasses[possibleLineClasses %>% map_lgl(function(x){lines[[i, x]]})]

      if(lines[[i, "startUninterestingGroup"]]){
        retList[[length(retList)+1]] <- tags$tr(class="uninteresting-group-ellipsis",
          tags$td(`data-line-number` = "…", class="line-number"), tags$td(""))
      }

      retList[[length(retList)+1]] <- tags$tr(class=paste(lineClasses, collapse=" "),
        tags$td(`data-line-number` = i, class="line-number"), tags$td(class="line", lines[[i, 'text']]))
    }

    retList[[length(retList)+1]] <- retList[[1]]

    uninterestingGroupToggleId <- paste0("uninterestingGroupToggle", strftime(Sys.time(), "%y%m%d%H%M%S"), length(retList))

    return(tags$div(
      tags$input(type="checkbox", checked=TRUE, id=uninterestingGroupToggleId, class="rumba-logviewer-uninteresting-group-toggle"),
      tags$label(`for`=uninterestingGroupToggleId, "Collapse groups of uninteresting lines"),
      tags$table(class="rumba-logviewer", retList)
    ))
  }))
}