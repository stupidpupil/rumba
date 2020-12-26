library(R6)
library(tidyverse)
library(lubridate)

RumbaAuthzGroup <- R6Class("RumbaAuthzGroup", list(

  name = NULL,
  authzGroupsPath = "authz_groups",

  adObjects = tibble(
    sAMAccountName = character(0),
    objectType = factor(levels=c("user", "group")),
    addedAt = POSIXct(0)
  ),


  initialize = function(name){

    stopifnot(is.character(name))
    stopifnot(name %>% str_detect("^[A-Za-z_0-9]{3,64}$"))
    self$name <- name

    self$load()
  },

  load = function(){
    csvPath <- self$getCSVPath()

    if(!file.exists(csvPath)){
      return(FALSE)
    }

    tryCatch({
      new_objects <- read_csv(self$getCSVPath(), col_types='cfT')

      stopifnot((setdiff(colnames(new_objects), colnames(self$adObjects)) %>% length()) == 0)

      new_objects <- new_objects %>% mutate(
        objectType = factor(objectType, levels=levels(self$adObjects$objectType))
      )

      stopifnot(all(!is.na(new_objects$objectType)))

      self$adObjects <- new_objects

      return(TRUE)
    },
      error = function(err){print(err)}
    )

    return(FALSE)
  },

  save = function(){
    self$adObjects %>% write_csv(self$getCSVPath())
  },

  getCSVPath = function(){
    paste0(self$authzGroupsPath, "/", self$name, ".csv")
  },

  addUser = function(username){

    # This regex is extremely conservative!
    if(!str_detect(username, "^[A-Za-z0-9_.]{1,64}$")){
      stop("Invalid username")
    }

    if(username %in% self$adObjects$sAMAccountName){
      stop("Username already exists in this group")
    }

    if(Sys.info()[["sysname"]] == "Windows"){
      checkUsernameCommand <- paste0("net user ", username, " /domain")
    }else{ # Fallback for testing purposes

      if(username == "gerald"){
        checkUsernameCommand <- "echo ':)'"
      }else{
        checkUsernameCommand <- "echo 'could not be found'"
      }
    }

    checkUsernameOutput <- system(checkUsernameCommand, intern = TRUE)

    if(str_detect(checkUsernameOutput, "could not be found")){
      stop("Username could not be found")
    }

    self$adObjects <- self$adObjects %>% add_row(
      sAMAccountName = username,
      objectType = "user",
      addedAt = now()
    )
  },

  addGroup = function(groupname){

    # This regex is extremely conservative!
    if(!str_detect(groupname, "^[A-Za-z0-9_.]{1,64}$")){
      stop("Invalid groupname")
    }

    if(groupname %in% self$adObjects$sAMAccountName){
      stop("Groupname already exists in this group")
    }

    if(Sys.info()[["sysname"]] == "Windows"){
      checkGroupnameCommand <- paste0("net group ", groupname, " /domain")
    }else{ # Fallback for testing purposes
      checkGroupnameCommand <- "echo 'could not be found'"
    }

    checkGroupnameOutput <- system(checkGroupnameCommand, intern = TRUE)

    if(str_detect(checkGroupnameOutput, "could not be found")){
      stop("Groupname could not be found")
    }

    self$adObjects <- self$adObjects %>% add_row(
      sAMAccountName = groupname,
      objectType = "group",
      addedAt = now()
    )
  },

  removeObject = function(name_to_remove){
    self$adObjects <- self$adObjects %>% filter(sAMAccountName != name_to_remove)
  }



))
