tibbleLists <- function(objExample){
  nNode <- as.integer()
  nTip <- as.integer()
  fromNode <- as.integer()
  tooNode <- as.integer()
  tipLabel <- as.character()
  nodeLabel <- ("Base")
  
  tempAtt <- attributes(summary(objExample))
  print(tempAtt$dimnames)
  #print(tempAtt$dim[[1]])
  
  for (i in 1:tempAtt$dim[[1]]){
    if(is.list(objExample[[i]])&& !inherits(objExample[[i]], "data.frame")){
      tempAtt2<-attributes(summary(objExample[[tempAtt$dimnames[[1]][[i]]]]))
      nNode <- nNode + 1
      nodeLabel <- c(nodeLabel, tempAtt$dimnames[[1]][[i]])
      subNode <- subList(objExample[[i]])
      
      
      nNode <- nNode + subNode$nNode 
      nodeLabel <- c(nodeLabel,subNode$nodeLabel)
      tipLabel <- c(tipLabel, subNode$tipLabel)
      
      #this needs to be changed:
      fromNode <- c(fromNode, subNode$fromNode)
      tooNode <- c(tooNode, subNode$tooNode)
    }
    else{
      nTip <- nTip + 1
      tipLabel <- c(tipLabel, tempAtt$dimnames[[1]][[i]])
    }
  }
  
  
  ## this will be challenging
  edges <- matrix(c(t(fromNode),t(tooNode)),ncol=2)
  
  ##
  treeSection <- list(tipLabel = tipLabel
                      , nodeLabel = nodeLabel
                      , nNode = nNode
                      , nTip = nTip
                      , edges = edges)
  return(treeSection)
}

subList <- function(objExample){
  nNode <- as.integer(0)
  nTip <- as.integer(0)
  tipLabel <- as.character()
  nodeLabel <- ("Branch")
  tipNo <- 0
  nodeNo <- 100
  fromNode <- as.integer()
  tooNode <- as.integer()
  tempAtt <- attributes(summary(objExample))
  for (i in 1:tempAtt$dim[[1]]){
    if(is.list(objExample[[i]])&& !inherits(objExample[[i]], "data.frame")){
      tempAtt2<-attributes(summary(objExample[[tempAtt$dimnames[[1]][[i]]]]))
      nNode <- nNode + 1
      nodeLabel <- c(nodeLabel, tempAtt$dimnames[[1]][[i]])
      tooNode <- c(tooNode,(10*i+1):(10*i+tempAtt$dim[[1]]))
      fromNode <- c(fromNode, rep(100+i,tempAtt$dim[[1]]))
    }
    else{
      nTip <- nTip + 1
      fromNode <- c(fromNode,100+i)
      tooNode <- c(tooNode,10*i + 1)
      tipLabel <- c(tipLabel, tempAtt$dimnames[[1]][[i]])
    }
  }
  branchSection <- list(tipLabel = tipLabel, 
                        nodeLabel = nodeLabel, 
                        nNode = nNode, 
                        nTip = nTip,
                        fromNode=fromNode, 
                        tooNode = tooNode)
  
  return(branchSection)
}



tempTree <- tibbleLists(mrIML_mlp)
#print(tempTree)




