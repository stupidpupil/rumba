library(R6)
library(tidyverse)

source("rumba_authz_group.R")

RumbaAuthzController <- R6Class("RumbaAuthzController", list(


  authzGroupDidUpdate = function(authzGroupName){
    for(app in rumba_apps_unreactive){
      if(authzGroupName %in% app$options$allow){
        self$updateAuthorizationForApp(app)
      } 
    }
  },

  updateAuthorizationForApp = function(app){
    webConfig <- RumbaIISWebConfig$new(app)
    webConfig$insertOrUpdateAuthorizationWithUsersAndGroups(self$adUsersAndGroupsForApp(app))
  },


  adUsersAndGroupsForApp = function(app){

    if(is.null(app$options$allow)){
      return(NULL)
    }

    ret_tibble <- tibble()

    for(authzGroupName in app$options$allow){
      ret_tibble <- ret_tibble %>% rbind(RumbaAuthzGroup$new(authzGroupName)$adObjects)
    }

    ret_tibble %>% select(sAMAccountName, objectType) %>% unique
  }


))