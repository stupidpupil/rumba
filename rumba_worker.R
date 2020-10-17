library(R6)
library(processx)

RumbaWorker <- R6Class("RumbaWorker", list(

  appName = NULL,
  appDir = NULL,
  basePort = NULL,
  workerIndex = NULL,


  process = NULL,
  state = "stopped",
  latestLogPath = NULL,

  initialize = function(appName, appDir, basePort, workerIndex){
    stopifnot(is.character(appName))
    stopifnot(is.character(appDir), file.exists(appDir))
    stopifnot(is.numeric(basePort), basePort %in% 5001:12001)
    stopifnot(is.numeric(workerIndex), workerIndex %in% 1:100)

    self$appName <- appName
    self$appDir <- appDir
    self$basePort <- as.integer(basePort)
    self$workerIndex <- as.integer(workerIndex)
  },


  # IIS Server Farm insists on different server names
  # so we go through the 127.0.0.1-100 loopback range
  getHost = function(){

    # But if we're testing on macOS
    # this only sets up 127.0.0.1 on the loopback
    if(Sys.info()[["sysname"]] == "Darwin"){
      return("127.0.0.1")
    }

    paste0("127.0.0.", self$workerIndex)
  },

  getPort = function(){
    self$basePort + (self$workerIndex-1L)
  },

  getRCommand = function(){
    paste0(
      "shiny::runApp('", self$appDir, "',",
      "host='", self$getHost(),"',",
      "port=", self$getPort(),")"
    )
  },

  start = function(){
    if(!(self$state %in% c("stopped", "failed"))){
      return(FALSE)
    }

    self$state <- "starting"

    tryCatch({
      binPath <- ps::ps_exe(ps::ps_handle())

      if(Sys.info()[["sysname"]] == "Darwin"){
        binPath <- "/opt/local/bin/R" #HACK
      }


      logDir <- paste0("./logs/", self$appName, "/", self$workerIndex)

      if(!file.exists(logDir)){
        dir.create(logDir, recursive = TRUE, showWarnings = TRUE)
      }else{
        existingLogs <- list.files(logDir)
        if(length(existingLogs) > 4)
        file.remove(paste(logDir, existingLogs[1:(length(existingLogs) -4)], sep="/"))
      }

      self$latestLogPath <- paste0(logDir, "/" , format(Sys.time(), "%Y%m%dT%H%M%S", tz="UTC"), " ", self$appName, "-", self$workerIndex, ".log")


      self$process <- process$new(
        binPath,
        c("-e", self$getRCommand()),
        stdout=self$latestLogPath,
        stderr="2>&1",
        supervise=TRUE,
        cleanup_tree=TRUE
      )
    },
      error = function(err){
        print(err)
        self$state <- "failed"
      }
    )

    if(self$state != "failed"){
      return(TRUE)
    }

    return(FALSE)
  },

  stop = function(){
    if(!(self$state %in% c("started", "starting", "stopping", "failed"))){
      return(FALSE)
    }

    self$state <- "stopping"

    tryCatch({
      if(!is.null(self$process)){
        self$process$kill_tree()
      }

    },

      error = function(err){
        print(err)
      }
    )

    return(TRUE)
  },


  logHasListening = function(){

    if(is.null(self$latestLogPath)){
      return(FALSE)
    }

    if(!file.exists(self$latestLogPath)){
      return(FALSE)
    }

    lookingFor <- paste0("Listening on http://",self$getHost(),":",self$getPort())

    any(readLines(self$latestLogPath) == lookingFor)
  },

  tick = function(){



    if(self$state == "stopping"){
      if(!self$process$is_alive()){
        self$state <- "stopped"
      }
    }

    if(self$state == "started"){
      if(!self$process$is_alive()){
        self$state <- "failed"
      }
    }

    if(self$state == "starting"){
      if(self$process$is_alive()){
        if(self$logHasListening()){
          self$state <- "started"
        }
      }else{
        if(!is.null(self$process$get_exit_status())){
          self$state <- "failed"
        }
      }
    }

  },

  # Resident set size
  getRSS = function(){
    if(!(self$state %in% c("starting", "started", "stopping"))){
      return(NA_real_)
    }

    if(!self$process$is_alive()){
      return(NA_real_)
    }

    tryCatch({

      return(self$process$get_memory_info()[['rss']])

      }
      ,error = function(err){
        print(err)
        return(NA_real_)
      }

    )

  }

  #,finalize = function(){
  #  if(!is.null(self$process)){
  #    self$process$kill_tree()
  #  }
  #}

))