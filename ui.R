dashboardPage(
  dashboardHeader(title =  "Rumba"),
  dashboardSidebar(
    sidebarMenuOutput("sidebarMenuOut")
    ),
  dashboardBody(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "rumba.css")
      ),


    tabItems(
      tabItem(tabName="apps",
        fluidRow(
          box(width=12,

            actionButton("buttonScanForNewApp", "Scan for New Apps"),
            actionButton("buttonStartAllApps", "Start All Apps"),
            actionButton("buttonStopAllApps", "Stop All Apps"),

            uiOutput("uiTableRumbaApps")
            )

          )
        ),

      tabItem(tabName="appDetails",
        fluidRow(
          box(width=12,
            selectInput("selectApp", "App", choices=c(), selectize=FALSE)
          )
        ),

        fluidRow(
          box(width=8,
            uiOutput("uiSelectedAppInvalidError"),
            uiOutput("uiSelectedAppDetails")
          ),

          box(width=4,
            uiOutput("uiSelectedAppButtons"),
            uiOutput("uiTableSelectedAppRumbaWorkers")
          )

        )
      )

    )
  )
)