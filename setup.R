for (package in c("tidyverse", "shiny", "shinydashboard", "R6", "xml2" ,"yaml", "shinyloadtest")) {
  if (!package %in% installed.packages()) {
    install.packages(package)
  }
}
