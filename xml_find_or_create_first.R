library(xml2)
library(stringr)
library(magrittr)

#
# @param pseudoXPath A path including currentNode, like "./system.webServer/rewrite/rules"
#
xml_find_or_create_first = function(currentNode, pseudoXPath){

  if(pseudoXPath == "."){
    return(currentNode)
  }

  pseudoXPathComponents <- str_split(pseudoXPath, "/")[[1]] %>% unlist()

  countComponents <- length(pseudoXPathComponents)


  if(pseudoXPathComponents[[countComponents-1]] != ""){
    currentNode <- xml_find_or_create_first(currentNode, pseudoXPathComponents[1:countComponents-1] %>% paste(collapse="/"))

    newNode <- xml_find_first(currentNode, paste0("./", pseudoXPathComponents[countComponents]))

    if(is.na(newNode)){
      newNode <- xml_add_child(currentNode, pseudoXPathComponents[countComponents])
    }

    return(newNode)
  }

  # pseudoXPath either looks like "/configuration" or "//configuration"

  if(pseudoXPath %>% str_detect("^//")){

    if(intersect(class(currentNode), c("xml_node", "xml_document")) %>% 
      setequal(c("xml_document"))){
      # Then currentNode is actually an empty XML document
      newNode <- xml_add_child(currentNode, pseudoXPathComponents[countComponents])
      return(newNode)
    }

    # This might return xml_missing() if the root node doesn't match
    return(xml_find_first(currentNode, pseudoXPath))
  }


  return(xml_find_or_create_first(currentNode, paste0(".", pseudoXPath)))
}