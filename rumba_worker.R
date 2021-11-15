library(R6)
library(processx)

RumbaWorker <- R6Class("RumbaWorker", list(

  app = NULL,
  basePort = NULL,
  workerIndex = NULL,

  herbst = NULL,
  process = NULL,
  state = "stopped",
  shinyState = "stopped",
  latestLogPath = NULL,

  activeWsClients = 0,
  lastWsClientSeen = Sys.time(),
  shinyStartedAt = Sys.time(),

  initialize = function(app, basePort, workerIndex){
    stopifnot(is.numeric(basePort), basePort %in% 5001:12001)
    stopifnot(is.numeric(workerIndex), workerIndex %in% 1:100)

    self$app <- app
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
    self$basePort + 2*(self$workerIndex-1L)
  },

  getShinyPort = function(){
    self$basePort + 2*(self$workerIndex-1L) + 1L
  },

  getClaimedPorts = function(){
    c(self$getPort(), self$getShinyPort())
  },

  getRCommand = function(){
    paste0(
      "shiny::runApp('", self$app$appDir, "',",
      "host='", self$getHost(),"',",
      "port=", self$getShinyPort(),")"
    )
  },

  start = function(){

    if(!(self$state %in% c("stopped", "failed"))){
      return(FALSE)
    }

    self$state <- "starting"

    self$herbst <- httpuv::startServer(self$getHost(), self$getPort(),
      list(
        call = function(req){

           if(self$shinyState != "started"){
              self$startShiny()

              progress_value <- as.double(difftime(Sys.time(), self$shinyStartedAt, units="secs"))
              progress_value <- progress_value + 1
              
              progress_max <- self$app$shinyStartupEstimateSeconds
              progress_max <- progress_max + 1
              progress_max <- max(progress_max/0.95, progress_value+3)
            }

          if(req$PATH_INFO == "/.rumba/progress"){
            status_resp = list(
              status = 200L,
              headers = list('Content-Type' = 'text/plain')
            )

            if(self$shinyState != "started"){
              progress_est <- floor(100*progress_value/progress_max)
              status_resp$body <- as.character(progress_est)
            }else{
              status_resp$body <- "100"
            }

            return(status_resp)
          }

          if(self$shinyState != "started"){
            failed_resp <- list(
              status = 200L,
              headers = list('Content-Type' = 'text/html'),
              body = rumba_reloader_html(
                progress_max = progress_max,
                progress_value = progress_value
                )
            )

            return(failed_resp)
          }

          req_curl <- shinyloadtest:::req_rook_to_curl(req, paste0(self$getHost(),":",self$getShinyPort()))



          targetURL <- shinyloadtest:::URLBuilder$new(paste0("http://",self$getHost(),":", self$getShinyPort()))
          query <- gsub("\\?", "", req$QUERY_STRING)
          url <- targetURL$appendPath(req$PATH_INFO)$setQuery(query)$build()

          h <- curl::new_handle()
          curl::handle_setheaders(h, .list = req_curl)

          if (!is.null(req$HTTP_CONTENT_LENGTH) && as.integer(req$HTTP_CONTENT_LENGTH) > 0) {

            curl::handle_setopt(h, post=TRUE, postfieldsize = as.integer(req$HTTP_CONTENT_LENGTH))
            req$rook.input$rewind()
            curl::handle_setopt(h, readfunction = function(n) {
              data <- req$rook.input$read(n)
              return(data)
            })
            req_type <- 'post'
          }


          tryCatch({
            resp_curl <- curl::curl_fetch_memory(url, handle = h)

          },
            error = function(err){
              #print(req_curl)
              #print(req$rook.input)
              print(err)}
          )


          shinyloadtest:::resp_httr_to_rook(resp_curl)
        },

        onWSOpen = function(ws_in){
          self$activeWsClients <- self$activeWsClients + 1
          msg_buffer <- list()
          ws_in$onMessage(function(binary, message){msg_buffer <<- c(msg_buffer, list(message))})

          ws_out <- websocket::WebSocket$new(
            paste0("ws://",self$getHost(),":", self$getShinyPort()),
            maxMessageSize = 512 * 1024 * 1024)

          ws_out$onOpen(function(open_event){
            ws_out$onMessage(function(msg_event){ws_in$send(msg_event$data)})
            ws_in$onMessage(function(binary, message){ws_out$send(message)})

            for (msg in msg_buffer) {
              ws_out$send(msg)
            }

            ws_in$onClose(function(){
              self$activeWsClients <- self$activeWsClients - 1

              if(self$activeWsClients == 0){
                self$lastWsClientSeen <- Sys.time()
              }

              ws_out$close()})

            ws_out$onClose(function(event){ws_in$close()})
            ws_out$onError(function(event){ws_in$close()})
            
          })
        }
      )
    )

    self$state <- "started"
  },

  stop = function(){

    if(!(self$state %in% c("starting", "started"))){
      return(FALSE)
    }

    self$state <- 'stopping'

    self$herbst$stop()
    self$stopShiny()

  },

  startShiny = function(){
    if(!(self$shinyState %in% c("stopped", "failed"))){
      return(FALSE)
    }

    self$shinyState <- "starting"
    self$shinyStartedAt <- Sys.time()

    tryCatch({
      binPath <- ps::ps_exe(ps::ps_handle())

      if(Sys.info()[["sysname"]] == "Darwin"){
        binPath <- "/opt/local/bin/R" #HACK
      }


      logDir <- paste0("./logs/", self$app$name, "/", self$workerIndex)

      if(!file.exists(logDir)){
        dir.create(logDir, recursive = TRUE, showWarnings = TRUE)
      }else{
        existingLogs <- list.files(logDir)
        if(length(existingLogs) > 4)
        file.remove(paste(logDir, existingLogs[1:(length(existingLogs) -4)], sep="/"))
      }

      self$latestLogPath <- paste0(logDir, "/" , format(Sys.time(), "%Y%m%dT%H%M%S", tz="UTC"), " ", self$app$name, "-", self$workerIndex, ".log")


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
        self$shinyState <- "failed"
      }
    )

    if(self$shinyState != "failed"){
      return(TRUE)
    }

    return(FALSE)
  },

  stopShiny = function(){
    if(!(self$shinyState %in% c("started", "starting", "stopping", "failed"))){
      return(FALSE)
    }

    self$shinyState <- "stopping"

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

    lookingFor <- paste0("Listening on http://",self$getHost(),":",self$getShinyPort())

    any(readLines(self$latestLogPath) == lookingFor)
  },

  getShinyIdleSeconds = function(){
    if(self$shinyState != 'started'){
      return(0L)
    }

    if(self$activeWsClients > 0){
      return(0L)
    }

    return(as.integer(difftime(Sys.time(), self$lastWsClientSeen, units="secs")))
  },

  getShinyEvictionPriority = function(){

    # Has to be idle for at least 15 seconds to be eligible
    if(self$getShinyIdleSeconds() < 15L){
      return(-1L)
    }

    priority <- 1

    priority <- priority +
      max(0, self$getShinyIdleSeconds() - self$app$shinyStartupEstimateSeconds)

    priority <- priority +
      self$getRSS()/(10*1024*1024)

    priority <- priority /
      1+sqrt(self$app$shinyStartupEstimateSeconds)

    return(priority)
  },

  tick = function(){

    if(self$shinyState == "stopping"){
      if(!self$process$is_alive()){
        self$shinyState <- "stopped"
      }
    }

    if(self$state == "stopping"){
      self$state <- self$shinyState
    }

    if(self$shinyState == "started"){
      if(!self$process$is_alive()){
        self$shinyState <- "failed"
      }
    }

    if(self$shinyState == "starting"){
      if(self$process$is_alive()){
        if(self$logHasListening()){
          Sys.sleep(0.5)
          self$app$updateShinyStartupEstimateSeconds(
            as.double(difftime(Sys.time(), self$shinyStartedAt, units="secs")))
          self$lastWsClientSeen <- Sys.time() + 120L # HACK to ensure that slow loading Shiny apps aren't slept immediately
          self$shinyState <- "started"
        }
      }else{
        if(!is.null(self$process$get_exit_status())){
          self$shinyState <- "failed"
        }
      }
    }

  },

  # Resident set size
  getRSS = function(){
    if(!(self$shinyState %in% c("starting", "started", "stopping"))){
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