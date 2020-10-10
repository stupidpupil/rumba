library(tidyverse)
library(yaml)


source("rumba_iis_application_host_config.R")
source("rumba_iis_web_config.R")

source("rumba_worker.R")
source("rumba_app.R")


rumba_options <- list(
  iisApplicationHostConfig = "C:/Windows/system32/inetsrv/config/applicationHost.config",
  iisWebConfig = "C:/inetpub/wwwroot/web.config",
  appsDir = "apps",
  webPrefix = "http://localhost/"
)

if(file.exists("config.yml")){
  rumba_options <- modifyList(rumba_options, yaml.load_file("config.yml"))
}

rumba_iis_application_host_config <- 
  RumbaIISApplicationHostConfig$new(rumba_options$iisApplicationHostConfig)
rumba_iis_application_host_config$removeAllRumbaWebFarms()

rumba_iis_web_config <-
  RumbaIISWebConfig$new(rumba_options$iisWebConfig)
rumba_iis_web_config$removeAllRumbaRewriteRules()

rumba_apps_unreactive <- list()

scan_for_rumba_apps <- function(){

  for (a in rumba_apps_unreactive) {
    a$reloadOptions() # This will just be ignored by any apps not in the right state
  }

  existingAppDirs <- rumba_apps_unreactive %>% map_chr(~.x$appDir)

  candidates <- Sys.glob(paste0(rumba_options$appsDir, "/*/rumba.yml")) %>% str_sub(1, -11)

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