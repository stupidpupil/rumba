source("renderLogviewer.R")

server <- function(input, output, session){


  output$sidebarMenuOut <- renderMenu({

    selectedItem <- isolate(input$sidebarMenu)

    sidebarMenu(id ="sidebarMenu",
      menuItem("Apps", tabName="apps", icon = icon("boxes"), selected=(selectedItem=="app"),
        badgeLabel=length(rumba_apps()), badgeColor = "light-blue"),
      menuItem("App details", tabName="appDetails", icon = icon("box-open"), selected=(selectedItem=="appDetails")),
      menuItem("Log viewer", tabName="logViewer", icon = icon("align-left"), selected=(selectedItem=="logViewer"))
    )
  })


  #
  # Apps Overview
  #

  output$uiTableRumbaApps <- renderUI({
    tags$table(class="table shiny-table table-spacing-s", style="width:100%; max-width:45em;",
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
            tags$td(actionLink(paste0("linkSelectApp", i), app$name)),
            tags$td(app$options$webPath),
            tags$td(uiOutput(paste0("textTableRumbaAppsStateTd", i), inline=TRUE)),
            tags$td(uiOutput(paste0("textTableRumbaAppsWorkerCountTd", i), inline=TRUE)),
            tags$td(uiOutput(paste0("textTableRumbaAppsMemoryTd", i), inline=TRUE))
          )})
      )
    )
  })

  uiTableRumbaAppsStateColumns = list(
    State = function(app){tags$span(class=paste0("rumba-state ", app$state), app$state)},
    WorkerCount = function(app){paste0(app$activeWorkerCount(), "/", app$options$workerCount)}
  )

  for(c in names(uiTableRumbaAppsStateColumns)){
    for (i in 1:(rumba_options$maxAppsUIElements)) {
      local({
        j <- i
        d <- c

        output[[paste0("textTableRumbaApps", d, "Td", i)]] <- renderUI({
          req(rumba_apps_with_tick_and_state())
          if(length(rumba_apps_with_tick_and_state()) < j){return("")}
          uiTableRumbaAppsStateColumns[[d]](rumba_apps_with_tick_and_state()[[j]])
        })
      })
    }
  }

  for (i in 1:(rumba_options$maxAppsUIElements)) {
    local({
      j <- i

      observeEvent(input[[paste0("linkSelectApp", j)]], {
        updateSelectInput(session, "selectApp", selected=j)
        updateTabItems(session, "sidebarMenu", selected="appDetails")
      })
    })
  }

  uiTableRumbaAppsResourceColumns = list(
    Memory = function(app){app$getRSS() %>% utils:::format.object_size(units="auto", standard = "IEC", digits=0L)}
  )

  for(c in names(uiTableRumbaAppsResourceColumns)){
    for (i in 1:(rumba_options$maxAppsUIElements)) {
      local({
        j <- i
        d <- c

        output[[paste0("textTableRumbaApps", d, "Td", i)]] <- renderUI({
          req(rumba_apps_with_resources())
          if(length(rumba_apps_with_resources()) < j){return("")}
          uiTableRumbaAppsResourceColumns[[d]](rumba_apps_with_resources()[[j]])
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
    names(choices) <- apps %>% map_chr(~.x$name)

    updateSelectInput(session, 'selectApp', selected=selected, choices=choices)
  })

  selectedRumbaApp <- reactive({
    req(rumba_apps())
    req(input$selectApp)
    rumba_apps()[[as.integer(input$selectApp)]]
  })

  selectedRumbaAppWithTickAndState <- reactive({
    req(rumba_apps_with_tick_and_state())
    req(input$selectApp)
    rumba_apps_with_tick_and_state()[[as.integer(input$selectApp)]]
  })

  selectedRumbaAppWithResources <- reactive({
    req(rumba_apps_with_resources())
    req(input$selectApp)
    rumba_apps_with_resources()[[as.integer(input$selectApp)]]
  })

  output$uiSelectedAppDetails <- renderUI({

    app <- selectedRumbaAppWithTickAndState()

    rows <- list()

    rows <- append(rows, list(
      tags$tr(tags$th("State"), tags$td(tags$span(class=paste0("rumba-state ", app$state), app$state))),
      tags$tr(tags$th("Directory"), tags$td(normalizePath(app$appDir)))
    ))

    if(app$state != "invalid"){
      rows <- append(rows, list(
        tags$tr(tags$th("Web Path"), tags$td(app$options$webPath)),
        tags$tr(tags$th("Worker Count"), tags$td(app$options$workerCount)),
        tags$tr(tags$th("Base Port"), tags$td(app$options$basePort)),
        tags$tr(tags$th("Web Farm Name"), tags$td(rumba_iis_application_host_config$webFarmNameForRumbaApp(app)))
      ))
    }


    tags$table(class="table shiny-table table-spacing-s", style="width:100%; max-width:45em;",
      tags$tbody(rows)
    )
  })

  output$uiSelectedAppInvalidError <- renderUI({
    if(selectedRumbaAppWithTickAndState()$state != "invalid"){
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

    })(selectedRumbaAppWithTickAndState()$invalidError))

  })


  output$uiSelectedAppButtons <- renderUI({

    state <- selectedRumbaAppWithTickAndState()$state

    if (state %in% c("stopped")){
      buttonStartApp <- actionButton("buttonStartApp", "Start")
    }else{
      buttonStartApp <- tags$button(class="btn", disabled="true", "Start")
    }

    if (state %in% c("stopped", "invalid")){
      buttonStopApp <- tags$button(class="btn", disabled="true", "Stop")
      buttonReloadApp <- actionButton("buttonReloadApp", "Reload")
    }else{
      buttonStopApp <- actionButton("buttonStopApp", "Stop")
      buttonReloadApp <- tags$button(class="btn", disabled="true", "Reload")

    }

    list(
      buttonStartApp,
      buttonStopApp,
      buttonReloadApp
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


  observeEvent(input$buttonReloadApp, {
    req(selectedRumbaApp())
    selectedRumbaApp()$reloadOptions()
  })


  output$uiTableSelectedAppRumbaWorkers <- renderUI({

    if(selectedRumbaApp()$state == "invalid"){return("")}

    tags$table(class="table shiny-table table-spacing-s", style="width:100%",
      tags$thead(
        tags$tr(
          tags$th("Host"),
          tags$th("Port"),
          tags$th("State"),
          tags$th("Memory")
        )
      ),

      tags$tbody(
        selectedRumbaApp()$workers %>% imap(function(w, i){
          tags$tr(
            tags$td(class="rumba-host", w$getHost()),
            tags$td(class="rumba-port", w$getPort()),
            tags$td(uiOutput(paste0("textTableSelectedAppRumbaWorkersStateTd", i), inline=TRUE)),
            tags$td(uiOutput(paste0("textTableSelectedAppRumbaWorkersMemoryTd", i), inline=TRUE))
          )})
      )
    )
  })


  uiTableSelectedAppRumbaWorkersStateColumns = list(
    State = function(w){tags$span(class=paste0("rumba-state ", w$state), w$state)}
  )

  for(c in names(uiTableSelectedAppRumbaWorkersStateColumns)){
    for (i in 1:10) {
      local({
        j <- i
        d <- c

        output[[paste0("textTableSelectedAppRumbaWorkers", d, "Td", i)]] <- renderUI({
          req(selectedRumbaAppWithTickAndState())
          if(length(selectedRumbaAppWithTickAndState()$workers) < j){return("")}
          uiTableSelectedAppRumbaWorkersStateColumns[[d]](selectedRumbaAppWithTickAndState()$workers[[j]])
        })
      })
    }
  }


  uiSelectedAppRumbaWorkersResourceColumns = list(
    Memory = function(w){
      rss <- w$getRSS()
      if(is.na(rss)){return("")}
      rss %>% utils:::format.object_size(units="auto", standard = "IEC", digits=0L)}
  )

  for(c in names(uiSelectedAppRumbaWorkersResourceColumns)){
    for (i in 1:10) {
      local({
        j <- i
        d <- c

        output[[paste0("textTableSelectedAppRumbaWorkers", d, "Td", i)]] <- renderUI({
          req(selectedRumbaAppWithResources())
          if(length(selectedRumbaAppWithResources()$workers) < j){return("")}
          uiSelectedAppRumbaWorkersResourceColumns[[d]](selectedRumbaAppWithResources()$workers[[j]])
        })
      })
    }
  }


  #
  # Log viewer
  #

  observe({
    selected <- isolate(input$selectLogPath)
    updateSelectInput(session, "selectLogPath", choices=log_paths(), selected=selected)
  })


  logLines <- reactive({
    if(!(input$selectLogPath %in% log_paths())){
      return(NA_character_)
    }

    readLines(paste0("logs/", input$selectLogPath))

  })

  output$uiLogviewer <- renderLogviewer(logLines)


}