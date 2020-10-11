library(R6)
library(xml2)

RumbaIISApplicationHostConfig <- R6Class("RumbaIISApplicationHostConfig", list(


  applicationHostConfigPath = NULL,

  parentIISSiteName = "Rumba",
  rumbaRewriteRulePrefix = "Rumba Rule ",
  rumbaWebFarmPrefix = "Rumba ",


  initialize = function(applicationHostConfigPath){

    stopifnot(is.character(applicationHostConfigPath))
    stopifnot(file.exists(applicationHostConfigPath))
    # TODO: Check that it's a real applicationHost.config

    self$applicationHostConfigPath <- applicationHostConfigPath

  },

  applicationHostConfigXMLDoc = function(){
    read_xml(self$applicationHostConfigPath)
  },

  #
  # Web Farm
  #

  webFarmNameForRumbaApp = function(app){
    paste0(self$rumbaWebFarmPrefix, app$options$basePort, "-", app$options$workerCount)
  },

  webFarmXPathForRumbaApp = function(app){
    paste0("/configuration/webFarms/webFarm[@name='", self$webFarmNameForRumbaApp(app), "']") 
  },


  # TODO: Consider extracting this to its own class
  webFarmDocForRumbaApp = function(app){

    doc <- xml_new_root("webFarm", name=self$webFarmNameForRumbaApp(app), enabled="true")

    for(w in app$workers){
      wFs <- xml_add_child(doc, "server", address=w$getHost(), enabled="true")

      xml_add_child(wFs, "applicationRequestRouting", httpPort=w$getPort(), weight=1)
    }

    arr <- xml_add_child(doc, "applicationRequestRouting")

    xml_add_child(arr, "affinity", useCookie="true")
    xml_add_child(arr, "loadBalancing") #TODO: Check if this is necessary
    xml_add_child(arr, "healthCheck", url="http://localhost/")

    return(doc)
  },

  webFarmExistsForRumbaApp = function(app){
    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$webFarmXPathForRumbaApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    if(length(nodeSet) == 1){
      return(TRUE)
    }

    if(length(nodeSet) == 0){
      return(FALSE)
    }    

    stop(paste0("Multiple webFarm entries found for ", self$webFarmNameForRumbaApp(app)))
  },


  #TODO: Support multiple apps at the same time to reduce writes
  insertOrUpdateWebFarmForRumbaApp = function(app){ 

    doc <- self$applicationHostConfigXMLDoc()

    # Clear any existing entries for this app
    xPath <- self$webFarmXPathForRumbaApp(app)
    nodeSet <- xml_find_all(doc, xPath)
    xml_remove(nodeSet)

    webFarms <- xml_find_all(doc, "/configuration/webFarms")
    stopifnot(length(webFarms) == 1) # TODO: Consider inserting a webFarms element if missing

    webFarms <- webFarms[[1]]

    newWebFarm <- self$webFarmDocForRumbaApp(app)

    xml_add_child(webFarms, newWebFarm)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  },

  removeWebFarmForRumbaApp = function(app){

    if(!self$webFarmExistsForRumbaApp(app)){
      return(FALSE)
    }

    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$webFarmXPathForRumbaApp(app)

    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  },

  removeAllRumbaWebFarms = function(){
    doc <- self$applicationHostConfigXMLDoc()

    xPath = paste0("/configuration/webFarms/webFarm[starts-with(@name,'", self$rumbaWebFarmPrefix,"')]")

    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  },

  #
  # IIS Application
  #

  parentIISSiteXPath = function(){
    paste0("/configuration/system.applicationHost/sites/site[@name='", self$parentIISSiteName, "']")
  },

  iisApplicationXPathForRumbaApp = function(app){
    paste0(self$parentIISSiteXPath(), "/application[@path='", app$options$webPath, "']") 
  },

  iisApplicationDocForRumbaApp = function(app){
    doc <- xml_new_root("application", path=app$options$webPath)
    xml_add_child(doc, "virtualDirectory", path="/", physicalPath=normalizePath(rumba_options$iisApplicationsRoot))
    return(doc)
  },

  iisApplicationExistsForRumbaApp = function(app){
    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$iisApplicationXPathForRumbaApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    if(length(nodeSet) == 1){
      return(TRUE)
    }

    if(length(nodeSet) == 0){
      return(FALSE)
    }

    stop(paste0("Multiple IIS Application entries found for ", app$options$webPath))
  },

  removeIISApplicationForRumbaApp = function(app){

    if(!self$iisApplicationExistsForRumbaApp(app)){
      return(FALSE)
    }

    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$iisApplicationXPathForRumbaApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  },

  insertOrUpdateIISApplicationForRumbaApp = function(app){

    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$iisApplicationXPathForRumbaApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    parentSite <- xml_find_all(doc, self$parentIISSiteXPath())

    newIISApp <- self$iisApplicationDocForRumbaApp(app)

    xml_add_child(parentSite, newIISApp)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  },

  #
  # Location for IIS Application
  #

  iisApplicationLocationPathForApp = function(app){
    paste0(self$parentIISSiteName, "/", app$options$webPath)
  },

  iisApplicationLocationXPathForApp = function(app){
    paste0("/configuration/location[@path='", self$iisApplicationLocationPathForApp(app) ,"']")
  },


  rewriteRuleNameForRumbaApp = function(app){
    paste0(self$rumbaRewriteRulePrefix, app$options$basePort, "-", app$options$workerCount)
  },

  iisApplicationLocationDocForRumbaApp = function(app){

    location <- xml_new_root("location", path=self$iisApplicationLocationPathForApp(app))


    locationWebserver <- xml_add_child(location, "system.webServer")

    rewriteRules <- xml_add_child(xml_add_child(locationWebserver, "rewrite"), "rules")

    rewriteRuleDoc <- xml_new_root("rule", name=self$rewriteRuleNameForRumbaApp(app), stopProcessing="true")

    xml_add_child(rewriteRuleDoc, "match", url=paste0("^", app$options$webPath, "/(.*)"))
    xml_add_child(rewriteRuleDoc, "conditions", logicalGrouping="MatchAll", trackAllCaptures="false")

    webFarmName <- self$webFarmNameForRumbaApp(app)

    xml_add_child(rewriteRuleDoc, "action", type="Rewrite", url=paste0("http://",webFarmName ,"/{R:1}"))

    xml_add_child(rewriteRules, rewriteRuleDoc)

    return(location)
  },

  iisApplicationLocationExistsForApp = function(app){
    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$iisApplicationLocationXPathForApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    if(length(nodeSet) == 1){
      return(TRUE)
    }

    if(length(nodeSet) == 0){
      return(FALSE)
    }

    stop(paste0("Multiple Location entries found for ", self$iisApplicationLocationPathForApp(app)))
  },

  removeIISApplicationLocationForRumbaApp = function(app){

    if(!self$iisApplicationLocationExistsForApp(app)){
      return(FALSE)
    }

    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$iisApplicationLocationXPathForApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  },

  insertOrUpdateIISApplicationLocationForRumbaApp = function(app){

    doc <- self$applicationHostConfigXMLDoc()
    xPath <- self$iisApplicationLocationXPathForApp(app)
    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    configuration <- xml_find_all(doc, "/configuration")

    newIISAppLocation <- self$iisApplicationLocationDocForRumbaApp(app)

    xml_add_child(configuration, newIISAppLocation)

    write_xml(doc, self$applicationHostConfigPath)

    return(TRUE)
  }

))