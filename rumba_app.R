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

    self$argumentOptions <- list(...)

    self$reloadOptions()

  },

  reloadOptions = function(){

    if(!(self$state %in% c("stopped", "invalid"))){
      return(FALSE)
    }

    tryCatch({
      stopifnot(file.exists(self$appDir))

      self$options <- self$defaultOptions

      configPath <- paste0(self$appDir, "/rumba.yml")
      if(file.exists(configPath)){
        configYaml <- yaml.load_file(configPath)

        if(is.null(configYaml)){
          configYaml <- list()
        }

        self$options <- modifyList(self$options, configYaml)
      }

      self$options <- modifyList(self$options, self$argumentOptions)

      stopifnot(is.numeric(self$options$workerCount), self$options$workerCount %in% 1:9)
      stopifnot(is.character(self$options$webPath), grepl("^[a-z0-9_]+$", self$options$webPath))

      if(!is.na(self$options$basePort)){
        stopifnot(is.numeric(self$options$basePort), self$options$basePort %in% 5001:6999)
      }else{
        self$options$basePort <- rumba_port_allocator$getDynamicBasePort(self$options$workerCount)
      }

      portRange <- self$options$basePort:(self$options$basePort + self$options$workerCount - 1)
      rumba_port_allocator$stopIfPortsAreNotFree(portRange)

      self$invalidError <- NULL
      self$state <- "stopped"

      self$initializeWorkers()
    },

    error = function(err){
      print(err)
      self$options$workerCount = 0L
      self$invalidError <- err
      self$state <- "invalid"
    }

    )

    return(TRUE)
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

  getClaimedPorts = function(){
    sapply(self$workers, function(w){w$getPort()})
  },

  start = function(){

    if(self$state != "stopped"){
      return(FALSE)
    }

    self$state <- "starting"

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

    return(TRUE)
  },

  stop = function(){

    if(!(self$state %in% c("started", "starting", "stopping", "failed"))){
      return(FALSE)
    }

    self$state <- "stopping"

    tryCatch({
      rumba_iis_web_config$removeRewriteRuleForRumbaApp(self)
      rumba_iis_application_host_config$removeWebFarmForRumbaApp(self)
    },
      error = function(err){print(err)}
    )

    for (w in self$workers) {
      w$stop()
    }

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
    }

    wstates <- sapply(self$workers, function(w){w$state})

    if(self$state == "starting"){
      if(all(wstates == "started")){
        self$state <- "started"
      }
    }

    if(self$state == "stopping"){
      if(all(wstates == "stopped")){
        self$state <- "stopped"
      }
    }

    if(any(wstates == "failed")){
      self$state <- "failed"
    }

  }

  #,finalize = function(){
  #  for (w in self$workers) {
  #    w$stop()
  #  }
  #}

))