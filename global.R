library(tidyverse)
library(yaml)

library(shiny)
library(shinydashboard)

source("rumba_port_allocator.R")
source("rumba_authz_controller.R")

source("rumba_iis_application_host_config.R")
source("rumba_iis_web_config.R")

source("rumba_reloader_html.R")

source("rumba_worker.R")
source("rumba_app.R")

rumba_options <- list(
  iisApplicationHostConfig = "C:/Windows/system32/inetsrv/config/applicationHost.config",
  iisSitePath = "C:/inetpub/wwwroot",
  appsDir = "apps",
  webPrefix = "http://localhost/",
  maxAppsUIElements = 100L,
  evictIfRSSGreaterThanMiB = 512
)

if(file.exists("config.yml")){
  rumba_options <- modifyList(rumba_options, yaml.load_file("config.yml"))
}

rumba_port_allocator <- RumbaPortAllocator$new()
rumba_authz_controller <- RumbaAuthzController$new()

rumba_iis_application_host_config <- 
  RumbaIISApplicationHostConfig$new(rumba_options$iisApplicationHostConfig)
rumba_iis_application_host_config$removeAllRumbaWebFarms()


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


rumba_apps <- reactivePoll(500, NULL,

  checkFunc = function(){
    list(
      dirs = rumba_apps_unreactive %>% map(~.x$appDir),
      options = rumba_apps_unreactive %>% map(~.x$options)
    )
  },

  valueFunc = function(){
    rumba_apps_unreactive
  }
)

rumba_apps_with_tick_and_state <- reactivePoll(750, NULL,
  checkFunc = function(){

    for (app in rumba_apps_unreactive) {app$tick()}

    workers <- sapply(rumba_apps_unreactive, function(app){app$workers}) %>% unlist()

    evictIfRSSGreaterThanBytes <- rumba_options$evictIfRSSGreaterThanMiB * 1024 * 1024
    if(sum(sapply(workers, function(w){w$getRSS()}), na.rm=TRUE) > evictIfRSSGreaterThanBytes){
      workers <- workers[rev(order(sapply(workers, function(w){w$getShinyEvictionPriority()})))]

      if(workers[[1]]$getShinyEvictionPriority() > 0L){
        workers[[1]]$stopShiny()
      }

    }

    list(
      dirs = rumba_apps_unreactive %>% map(~.x$appDir),
      options = rumba_apps_unreactive %>% map(~.x$options),
      state = rumba_apps_unreactive %>% map(~.x$state),
      active_workers = rumba_apps_unreactive %>% map(~.x$activeWorkerCount())
    )
  },

  valueFunc = function(){
    rumba_apps_unreactive
  }
)


rumba_apps_with_resources <- reactivePoll(2500, NULL,
  checkFunc = function(){  
    list(
      state = rumba_apps_unreactive %>% map(~.x$state),
      active_workers = rumba_apps_unreactive %>% map(~.x$activeWorkerCount()),
      mem = rumba_apps_unreactive  %>% map_dbl(~.x$getRSS()) %>% signif(5)
    )
  },

  valueFunc = function(){
    rumba_apps_unreactive
  }
)

log_paths <- reactivePoll(750, NULL,
  checkFunc = function(){list.files("logs", recursive=TRUE, pattern="*/*/*.log")},
  valueFunc = function(){list.files("logs", recursive=TRUE, pattern="*/*/*.log")}
)

authz_groups <- reactivePoll(750, NULL,
  checkFunc = function(){list.files("authz_groups", recursive=TRUE, pattern="*.csv")},
  valueFunc = function(){list.files("authz_groups", recursive=TRUE, pattern="*.csv") %>% str_sub(1L,-5L)}
)

onStop(function(){

  for (app in rumba_apps_unreactive) {
    app$stop()
  }

})