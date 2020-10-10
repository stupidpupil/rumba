library(R6)
library(yaml)

RumbaApp <- R6Class("RumbaApp", list(

  appDir = NULL,
  options = list(),


  defaultOptions = list(
      workerCount = 2L,
      basePort = NA_integer_
  ),
  argumentOptions = list(),

  workers = list(),
  state = "stopped",
  invalidError = NULL,

  name = NULL,

  initialize = function(appDir, ...){
    stopifnot(is.character(appDir))
    self$name = basename(appDir)
    self$appDir = appDir

    tryCatch({
      stopifnot(file.exists(appDir))

      self$options <- self$defaultOptions

      configPath <- paste0(appDir, "/rumba.yml")
      if(file.exists(configPath)){
        self$options <- modifyList(self$options, yaml.load_file(configPath))
      }

      self$argumentOptions <- list(...)

      self$options <- modifyList(self$options, self$argumentOptions)

      print(self$options$webPath)

      stopifnot(is.numeric(self$options$workerCount), self$options$workerCount %in% 1:100)
      stopifnot(is.numeric(self$options$basePort), self$options$basePort %in% 5001:12001)
      stopifnot(is.character(self$options$webPath), grepl("^[a-z0-9_]+$", self$options$webPath))

      self$initializeWorkers()

    },

    error = function(err){
      print(err)
      self$options$workerCount = 0L
      invalidError <- err
      self$state <- "invalid"
    }

    )
  },

  initializeWorkers = function(){
    if(self$state != "stopped"){
      return(FALSE)
    }

    self$workers = list()

    for (i in 1L:self$options$workerCount) {
      self$workers[[i]] <-
        RumbaWorker$new(
          self$name,
          self$appDir,
          self$options$basePort,
          i
        )
    }
  },

  activeWorkerCount = function(){
    i <- 0L

    for (w in self$workers) {
      if(w$state == "started"){
        i <- i +1L
      }
    }

    return(i)
  },

  start = function(){

    if(self$state != "stopped"){
      return(FALSE)
    }

    tryCatch({
      for (w in self$workers) {
        w$start()

        if(w$state == "failed"){
          stop("Worker failed in start")
        }

      }

      rumba_iis_application_host_config$insertOrUpdateWebFarmForRumbaApp(self)
      rumba_iis_web_config$insertOrUpdateRewriteRuleForRumbaApp(self)

    },
      error = function(err){
        print(err)
        self$state <- "failed"
      }
    )

    if(self$state == "failed"){
      return(FALSE)
    }

    self$state <- "started"
    return(TRUE)
  },

  stop = function(){

    if(!(self$state %in% c("started", "starting", "stopping", "failed"))){
      return(FALSE)
    }

    tryCatch({
      rumba_iis_web_config$removeRewriteRuleForRumbaApp(self)
      rumba_iis_application_host_config$removeWebFarmForRumbaApp(self)
    },
      error = function(err){print(err)}
    )

    for (w in self$workers) {
      w$stop()
    }

    self$state <- "stopped"
    return(TRUE)
  },

  getRSS = function(){
    rss <- 0L

    for (w in self$workers) {

      wRSS <- w$getRSS()

      if(!is.na(wRSS)){
        rss <- rss + wRSS
      }

    }

    return(rss)
  },

  tick = function(){

    for (w in self$workers) {
      w$tick()

      if(w$state == "failed"){
        self$state <- "failed"
      }

    }
  }

  #,finalize = function(){
  #  for (w in self$workers) {
  #    w$stop()
  #  }
  #}

))