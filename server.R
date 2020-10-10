library(shiny)
library(shinydashboard)

library(future)
library(promises)

server <- function(input, output, session){

  rumba_apps <- reactivePoll(1000, NULL,

    checkFunc = function(){
      for (app in rumba_apps_unreactive) {
        app$tick()
      }
      
      list(
        dirs = rumba_apps_unreactive %>% map_chr(~.x$appDir),
        options = rumba_apps_unreactive %>% map(~.x$options),
        active_workers = rumba_apps_unreactive %>% map_chr(~.x$activeWorkerCount()),
        state = rumba_apps_unreactive %>% map_chr(~.x$state),
        mem = rumba_apps_unreactive  %>% map_chr(~.x$getRSS())
      )
    },

    valueFunc = function(){
      rumba_apps_unreactive
    }

  )

  # Won't react to changing state or memory etc
  # but will react to changing config
  rumba_apps_without_tick <- reactivePoll(500, NULL,

    checkFunc = function(){
      list(
        dirs = rumba_apps_unreactive %>% map_chr(~.x$appDir),
        options = rumba_apps_unreactive %>% map(~.x$options)
      )
    },

    valueFunc = function(){
      rumba_apps_unreactive
    }

  )



  output$tableRumbaApps <- renderTable({

    apps <- rumba_apps()

    tibble(
      Name = apps %>% map_chr(~.x$name),
      `Base Port` = apps %>% map_chr(~.x$options$basePort) %>% as.integer(),
      Workers = paste0(
        apps %>% map_chr(~.x$activeWorkerCount()), "/",
        apps %>% map_chr(~.x$options$workerCount)),
      State = apps %>% map_chr(~.x$state),
      Memory = apps %>% map_chr(function(x){x$getRSS() %>% utils:::format.object_size(units="auto", standard = "IEC")})
    )
  }, width="100%")


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

    req(rumba_apps_without_tick())

    apps <- rumba_apps_without_tick()

    choices <- 1:length(apps)
    names(choices) <- paste0(1:length(apps), " - ", apps %>% map_chr(~.x$name))

    updateSelectInput(session, 'selectApp', selected=selected, choices=choices)
  })

  selectedRumbaAppWithoutTick <- reactive({
    req(rumba_apps_without_tick())
    req(input$selectApp)
    rumba_apps_without_tick()[[as.integer(input$selectApp)]]
  })

  selectedRumbaApp <- reactive({
    req(rumba_apps())
    req(input$selectApp)
    rumba_apps()[[as.integer(input$selectApp)]]
  })

  output$uiSelectedApp <- renderUI({
    div(
      h2(id="selectedAppName", selectedRumbaAppWithoutTick()$name),
      p(id="selectedAppDir", normalizePath(selectedRumbaAppWithoutTick()$appDir)),
      p(
        a(id="selectedAppWebPath", 
          href=paste0(rumba_options$webPrefix, selectedRumbaAppWithoutTick()$options$webPath),
          target="_blank",
          selectedRumbaAppWithoutTick()$options$webPath
          )
      )
    )
  })

  observeEvent(input$buttonStartApp, {
    req(selectedRumbaApp())
    selectedRumbaApp()$start()
  })

  observeEvent(input$buttonStopApp, {
    req(selectedRumbaApp())
    selectedRumbaApp()$stop()
  })

  output$tableSelectedAppRumbaWorkers <- renderTable({
    ws <- selectedRumbaApp()$workers

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
    ws <- selectedRumbaAppWithoutTick()$workers
    choices <- 1:length(ws)
    names(choices) <- paste0(1:length(ws), " - ", ws %>% map_chr(~.x$getPort()))

    updateSelectInput(session, 'selectWorker', selected = selected, choices=choices)
  })

}