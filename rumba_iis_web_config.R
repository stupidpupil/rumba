library(R6)
library(xml2)

source("xml_find_or_create_first.R")

RumbaIISWebConfig <- R6Class("RumbaIISWebConfig", list(

  app = NULL,
  webConfigPath = NULL,
  rewriteRuleName = NULL,
  rewriteRuleXPath = NULL,
  
  rumbaRewriteRulePrefix = "Rumba Rule ",


  initialize = function(app){

    self$app <- app

    dir.create(paste0(rumba_options$iisSitePath, "/", app$options$webPath), recursive = FALSE)

    self$webConfigPath <- paste0(rumba_options$iisSitePath, "/", app$options$webPath, "/web.config")

    self$rewriteRuleName <- paste0(self$rumbaRewriteRulePrefix, app$options$basePort, "-", app$options$workerCount)

    self$rewriteRuleXPath <- paste0("/configuration/system.webServer/rewrite/rules/rule[@name='", self$rewriteRuleName, "']") 

  },

  webConfigDoc = function(){

    if(file.exists(self$webConfigPath)){
      return(read_xml(self$webConfigPath))
    }else{
      return(xml_new_root("configuration"))
    }
  },

  rewriteRuleDoc = function(){

    doc <- xml_new_root("rule", name=self$rewriteRuleName, stopProcessing="true")

    xml_add_child(doc, "match", url=paste0("^(.*)"))
    xml_add_child(doc, "conditions", logicalGrouping="MatchAll", trackAllCaptures="false")

    webFarmName <- rumba_iis_application_host_config$webFarmNameForRumbaApp(self$app)

    xml_add_child(doc, "action", type="Rewrite", url=paste0("http://",webFarmName ,"/{R:1}"))

    return(doc)
  },

  rewriteRuleExists = function(){
    doc <- self$webConfigDoc()
    xPath <- self$rewriteRuleXPath

    nodeSet <- xml_find_all(doc, xPath)

    if(length(nodeSet) == 1){
      return(TRUE)
    }

    if(length(nodeSet) == 0){
      return(FALSE)
    }    

    stop(paste0("Multiple rewrite rule entries found for ", self$self$rewriteRuleName))
  },

  removeRewriteRule = function(){

    if(!self$rewriteRuleExists()){
      return(FALSE)
    }

    doc <- self$webConfigDoc()
    xPath <- self$rewriteRuleXPath

    nodeSet <- xml_find_all(doc, xPath)

    xml_remove(nodeSet)

    write_xml(doc, self$webConfigPath)

    return(TRUE)
  },

  insertOrUpdateRewriteRule = function(){ 

    doc <- self$webConfigDoc()

    # Clear any existing entries for this app
    xPath <- self$rewriteRuleXPath
    nodeSet <- xml_find_all(doc, xPath)
    xml_remove(nodeSet)

    rewriteRules <- xml_find_or_create_first(doc, "//configuration/system.webServer/rewrite/rules")

    rewriteRules <- rewriteRules

    newRule <- self$rewriteRuleDoc()

    xml_add_child(rewriteRules, newRule)

    write_xml(doc, self$webConfigPath)

    return(TRUE)
  }

))