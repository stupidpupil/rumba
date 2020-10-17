library(shiny)
library(stringr)

renderLogviewer <- function(logLinesReactive){
  return(renderUI({
    
    retList <- list()
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
Type 'q()' to quit R.", "\n"))

    errorRegexp <- "^Error\\b"
    warningRegexp <- "^Warning\\b"

    positiveRegexp <- "^(> shiny::runApp|Listening on http)"

    for (i in 1:length(lines)) {
      line <- lines[[i]]


      lineClasses <- c()


      if(any(str_trim(line) == uninterestingLines)){
        lineClasses <- c(lineClasses, "uninteresting")
      }

      if(str_detect(line, errorRegexp)){
        lineClasses <- c(lineClasses, "error")
      }

      if(str_detect(line, warningRegexp)){
        lineClasses <- c(lineClasses, "warning")
      }

      if(str_detect(line, positiveRegexp)){
        lineClasses <- c(lineClasses, "positive")
      }

      retList[[i]] <- tags$code(class=lineClasses, line)
    }

    return(tags$div(tags$pre(class="rumba-logviewer", retList)))
  }))
}