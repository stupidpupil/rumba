server <- function(input, output, session){

  # Won't react to changing resources
  # but will react to changing config
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


  rumba_apps_with_tick_and_resources <- reactivePoll(1000, NULL,
    checkFunc = function(){
      for (app in rumba_apps_unreactive) {
        app$tick()
      }
      
      list(
        dirs = rumba_apps_unreactive %>% map(~.x$appDir),
        options = rumba_apps_unreactive %>% map(~.x$options),
        state = rumba_apps_unreactive %>% map(~.x$state),
        active_workers = rumba_apps_unreactive %>% map(~.x$activeWorkerCount()),
        mem = rumba_apps_unreactive  %>% map(~.x$getRSS())
      )
    },

    valueFunc = function(){
      rumba_apps_unreactive
    }
  )

  output$uiTableRumbaApps <- renderUI({
    tags$table(class="table shiny-table table- spacing-s", style="width:100%",
      tags$thead(
        tags$tr(
          tags$th("Name"),
          tags$th("Web Path"),
          tags$th("State"),
          tags$th("Workers"),
          tags$th("Memory")
        )
      ),

      tags$tbody(
        rumba_apps() %>% imap(function(app, i){
          tags$tr(
            tags$td(app$name),
            tags$td(app$options$webPath),
            tags$td(textOutput(paste0("textTableRumbaAppsStateTd", i))),
            tags$td(textOutput(paste0("textTableRumbaAppsWorkerCountTd", i))),
            tags$td(textOutput(paste0("textTableRumbaAppsMemoryTd", i)))
          )})
      )
    )
  })

  uiTableRumbaAppsResourceColumns = list(
    State = function(app){app$state},
    WorkerCount = function(app){paste0(app$activeWorkerCount(), "/", app$options$workerCount)},
    Memory = function(app){app$getRSS() %>% utils:::format.object_size(units="auto", standard = "IEC", digits=0L)}
  )

  for(c in names(uiTableRumbaAppsResourceColumns)){
    for (i in 1:(rumba_options$maxAppsUIElements)) {
      local({
        j <- i
        d <- c

        output[[paste0("textTableRumbaApps", d, "Td", i)]] <- renderText({
          uiTableRumbaAppsResourceColumns[[d]](rumba_apps_with_tick_and_resources()[[j]])
        })
      })
    }
  }

  #
  # Global actions
  #

  observeEvent(input$buttonStartAllApps, {
    for (app in rumba_apps()) {
      app$start()
    }
  })

  observeEvent(input$buttonStopAllApps, {
    for (app in rumba_apps()) {
      app$stop()
    }
  })

  observeEvent(input$buttonScanForNewApp, {
    scan_for_rumba_apps()
  })

  #
  # Selected App
  #

  observe({
    selected <- isolate(input$selectApp)

    req(rumba_apps())

    apps <- rumba_apps()

    choices <- 1:length(apps)
    names(choices) <- paste0(1:length(apps), " - ", apps %>% map_chr(~.x$name))

    updateSelectInput(session, 'selectApp', selected=selected, choices=choices)
  })

  selectedRumbaApp <- reactive({
    req(rumba_apps())
    req(input$selectApp)
    rumba_apps()[[as.integer(input$selectApp)]]
  })

  selectedRumbaAppWithTickAndResources <- reactive({
    req(rumba_apps_with_tick_and_resources())
    req(input$selectApp)
    rumba_apps_with_tick_and_resources()[[as.integer(input$selectApp)]]
  })

  output$uiSelectedApp <- renderUI({
    div(
      h2(id="selectedAppName", selectedRumbaApp()$name),
      p(id="selectedAppDir", normalizePath(selectedRumbaApp()$appDir)),
      p(
        a(id="selectedAppWebPath", 
          href=paste0(rumba_options$webPrefix, selectedRumbaApp()$options$webPath),
          target="_blank",
          selectedRumbaApp()$options$webPath
          )
      )
    )
  })

  output$uiSelectedAppInvalidError <- renderUI({
    if(selectedRumbaAppWithTickAndResources()$state != "invalid"){
      return(NULL)
    }

    p((function(err){

      if(is.null(err)){
        return("Unknown error")
      }

      if(is.null(err$message)){
        return("Unknown error")
      }

      if(err$message %>% str_detect("self\\$options\\$")){
        return(paste0(
          "Config error: ",
          err$message %>% str_replace_all("self\\$options\\$", ""))
        )
      }

      err$message

    })(selectedRumbaAppWithTickAndResources()$invalidError))

  })

  observeEvent(input$buttonStartApp, {
    req(selectedRumbaAppWithTickAndResources())
    selectedRumbaAppWithTickAndResources()$start()
  })

  observeEvent(input$buttonStopApp, {
    req(selectedRumbaAppWithTickAndResources())
    selectedRumbaAppWithTickAndResources()$stop()
  })

  output$tableSelectedAppRumbaWorkers <- renderTable({
    ws <- selectedRumbaAppWithTickAndResources()$workers

    tibble(
      Idx = ws %>% map_chr(~.x$workerIndex) %>% as.integer(),
      Host = ws %>% map_chr(~.x$getHost()),
      Port = ws %>% map_chr(~.x$getPort()) %>% as.integer(),
      State = ws %>% map_chr(~.x$state),
      Mem = ws %>% map_int(~.x$getRSS())
    )
  }, width="100%")

  #
  # Selected worker
  #

  observe({
    selected <- isolate(input$selectWorker)
    ws <- selectedRumbaApp()$workers
    choices <- 1:length(ws)
    names(choices) <- paste0(1:length(ws), " - ", ws %>% map_chr(~.x$getPort()))

    updateSelectInput(session, 'selectWorker', selected = selected, choices=choices)
  })

}