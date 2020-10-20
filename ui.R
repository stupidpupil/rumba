dashboardPage(
  dashboardHeader(title =  "Rumba"),
  dashboardSidebar(
    sidebarMenuOutput("sidebarMenuOut")
    ),
  dashboardBody(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "rumba.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "rumba_logviewer.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "rumba_authorization.css")
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
      ),


      tabItem(tabName="logViewer",
        fluidRow(
          box(width=12, selectInput("selectLogPath", "Log", choices=c(), selectize=FALSE))
        ),

        fluidRow(
          box(width=12, uiOutput("uiLogviewer"))
        )
      ),

      tabItem(tabName="authzGroups",
        fluidRow(
          box(width=6,
            selectInput("selectAuthzGroup", "Authorization Group", choices=c(), size=12, selectize=FALSE),
            textInput("textAuthzNewGroupName", "Group Name"),
            actionButton("buttonAuthzNewGroup", "New Group")
          ),
          box(width=6,
            uiOutput("uiTableAuthzGroupEntries"),
            textInput("textAuthzGroupNewNames", "User or Group Name"),
            actionButton("buttonAuthzGroupAddUser", "Add User"),
            actionButton("buttonAuthzGroupAddGroup", "Add Group")
          )
        )
      )


    )
  )
)