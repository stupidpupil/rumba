library(shiny)
library(tidyverse)

renderLogviewer <- function(logLinesReactive){
  return(renderUI({
    
    lines <- logLinesReactive()

    uninterestingLines <- unlist(str_split("
Copyright (C) 2020 The R Foundation for Statistical Computing
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
Loading required package: shiny", "\n"))

    errorRegexp <- "^Error\\b"
    warningRegexp <- "^Warning\\b"
    positiveRegexp <- "^Listening on http://"

    lines <- tibble(
      text = lines 
    ) %>%
    mutate(
      uninteresting = text %>% map_lgl(function(x){any(str_trim(x) == uninterestingLines)}),
      error = str_detect(text, errorRegexp),
      warning = str_detect(text, warningRegexp),
      positive = str_detect(text, positiveRegexp)
    ) %>%
    mutate(
      `uninteresting-group` = lag(uninteresting) & uninteresting & lead(uninteresting),
      startUninterestingGroup = !lag(`uninteresting-group`) & `uninteresting-group`
    )

    retList <- list()
    retList[[1]] <- tags$tr(class="bookend",
      tags$td(`data-line-number` = " ", class="line-number"), tags$td(class="line", ""))

    possibleLineClasses <- c("uninteresting", "error", "warning", "positive", "uninteresting-group")

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