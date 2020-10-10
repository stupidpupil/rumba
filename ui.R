dashboardPage(
  dashboardHeader(title =  "Rumba"),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "rumba.css")
    ),

    fluidRow(
      box(width=4,

        actionButton("buttonScanForNewApp", "Scan for New Apps"),
        actionButton("buttonStartAllApps", "Start All Apps"),
        actionButton("buttonStopAllApps", "Stop All Apps"),

        uiOutput("uiTableRumbaApps")
      ),
      box(width=4,
        selectInput("selectApp", "App", choices=c()),
        uiOutput("uiSelectedApp"),

        uiOutput("uiSelectedAppInvalidError"),

        actionButton("buttonStartApp", "Start"),
        actionButton("buttonStopApp", "Stop"),

        tableOutput("tableSelectedAppRumbaWorkers")
      ),

      box(width=4,
        selectInput("selectWorker", "Worker", choices=c())
      )
    )
  )
)