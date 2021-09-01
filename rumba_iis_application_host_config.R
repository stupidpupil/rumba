library(R6)
library(xml2)

RumbaIISApplicationHostConfig <- R6Class("RumbaIISApplicationHostConfig", list(


  applicationHostConfigPath = NULL,
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
    #xml_add_child(arr, "healthCheck", url="http://localhost/")

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
  }


))