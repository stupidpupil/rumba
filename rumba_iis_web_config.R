library(R6)
library(xml2)

RumbaIISWebConfig <- R6Class("RumbaIISWebConfig", list(

  webConfigPath = NULL,

  rumbaRewriteRulePrefix = "Rumba Rule ",


  initialize = function(webConfigPath){

    stopifnot(is.character(webConfigPath))
    stopifnot(file.exists(webConfigPath))

    self$webConfigPath <- webConfigPath

  },

  webConfigDoc = function(){
    read_xml(self$webConfigPath)
  },

  rewriteRuleNameForRumbaApp = function(app){
    paste0(self$rumbaRewriteRulePrefix, app$options$basePort, "-", app$options$workerCount)
  },

  rewriteRuleXPathForRumbaApp = function(app){
    paste0("/configuration/system.webServer/rewrite/rules/rule[@name='", self$rewriteRuleNameForRumbaApp(app), "']") 
  },

  rewriteRuleDocForRumbaApp = function(app){

    doc <- xml_new_root("rule", name=self$rewriteRuleNameForRumbaApp(app), stopProcessing="true")

    xml_add_child(doc, "match", url=paste0(app$options$webPath, "/(.*)"))
    xml_add_child(doc, "conditions", logicalGrouping="MatchAll", trackAllCaptures="false")

    webFarmName <- rumba_iis_application_host_config$webFarmNameForRumbaApp(app)

    xml_add_child(doc, "action", type="Rewrite", url=paste0("http://",webFarmName ,"/{R:1}"))

    return(doc)
  },

  rewriteRuleExistsForRumbaApp = function(app){
    doc <- self$webConfigDoc()
    xPath <- self$rewriteRuleXPathForRumbaApp(app)

    nodeSet <- xml_find_all(doc, xPath)

    if(length(nodeSet) == 1){
      return(TRUE)
    }

    if(length(nodeSet) == 0){
      return(FALSE)
    }    

    stop(paste0("Multiple rewrite rule entries found for ", self$rewriteRuleNameForRumbaApp(app)))
  },

  removeRewriteRuleForRumbaApp = function(app){

    if(!self$rewriteRuleExistsForRumbaApp(app)){
      return(FALSE)
    }

    doc <- self$webConfigDoc()
    xPath <- self$rewriteRuleXPathForRumbaApp(app)

    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$webConfigPath)

    return(TRUE)
  },

  #TODO: Support multiple apps at the same time to reduce writes
  insertOrUpdateRewriteRuleForRumbaApp = function(app){ 

    doc <- self$webConfigDoc()

    # Clear any existing entries for this app
    xPath <- self$rewriteRuleXPathForRumbaApp(app)
    nodeSet <- xml_find_all(doc, xPath)
    xml_remove(nodeSet)

    rewriteRules <- xml_find_all(doc, "/configuration/system.webServer/rewrite/rules")
    stopifnot(length(rewriteRules) == 1) # TODO: Consider inserting a rules element if missing

    rewriteRules <- rewriteRules[[1]]

    newRule <- self$rewriteRuleDocForRumbaApp(app)

    xml_add_child(rewriteRules, newRule)

    write_xml(doc, self$webConfigPath)

    return(TRUE)
  },


  removeAllRumbaRewriteRules = function(app){

    doc <- self$webConfigDoc()

    print(doc)

    xPath = paste0("/configuration/system.webServer/rewrite/rules/rule[starts-with(@name,'", self$rumbaRewriteRulePrefix,"')]")

    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$webConfigPath)

    return(TRUE)

  }

))