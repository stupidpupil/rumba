library(R6)


RumbaPortAllocator <- R6Class("RumbaPortAllocator", list(

  dynamicPortRange = 7001:12001,

  getClaimedPorts = function(){
    sapply(rumba_apps_unreactive, function(a){a$getClaimedPorts()})
  },

  stopIfPortsAreNotFree = function(ports){
    if(length(intersect(ports, self$getClaimedPorts())) > 0){
      stop("Ports conflict with other apps!")
    }
  },

  getDynamicBasePort = function(workerCount = 1){
    dynamicBasePorts <- Filter(function(x){x %% 10 == 1}, self$dynamicPortRange)

    claimedPorts <- self$getClaimedPorts()

    filterFunction <- function(basePort){
      desiredRange <- basePort:(basePort + workerCount - 1)

      if(length(intersect(desiredRange, self$dynamicPortRange)) != workerCount){
        return(FALSE)
      }

      length(intersect(desiredRange, claimedPorts)) == 0
    }

    dynamicBasePorts <- Filter(filterFunction, dynamicBasePorts)

    if(length(dynamicBasePorts) == 0){
      stop("No suitable dynamic base ports found!")
    }

    dynamicBasePorts[[1]]
  }

))