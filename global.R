library(tidyverse)

source("rumba_iis_application_host_config.R")
source("rumba_iis_web_config.R")

source("rumba_worker.R")
source("rumba_app.R")

rumba_iis_application_host_config <- 
  RumbaIISApplicationHostConfig$new("example/iis_configs/applicationHost.config")

rumba_iis_application_host_config$removeAllRumbaWebFarms()


rumba_iis_web_config <-
  RumbaIISWebConfig$new("example/iis_configs/web.config")

rumba_iis_web_config$removeAllRumbaRewriteRules()

rumba_apps_dir <- "./example/apps"

rumba_apps_unreactive <- list()

scan_for_rumba_apps <- function(){
  existingAppDirs <- rumba_apps_unreactive %>% map_chr(~.x$appDir)

  candidates <- Sys.glob(paste0(rumba_apps_dir, "/*/rumba.yml")) %>% str_sub(1, -11)

  candidates <- setdiff(candidates, existingAppDirs)

  for (appDir in candidates) {
    rumba_apps_unreactive[[length(rumba_apps_unreactive)+1]] <<- RumbaApp$new(appDir)
  }
}

scan_for_rumba_apps()

onStop(function(){

  for (app in rumba_apps_unreactive) {
    app$stop()
  }

})