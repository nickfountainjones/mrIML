
# reads a csv containing the default list of parsnip models.
knownModels<-read.csv("./data/ParsnipModels.csv")
#knownModels<-read.csv("./mrIML/data/ParsnipModels.csv")


parsnipModels <- knownModels[knownModels$Package %in% "parsnip",]
uniqueModels<-unique(parsnipModels$Model)


modeTypes <- unique(parsnipModels$Mode)


checkedModels <- data.frame(matrix(ncol = 2 + length(modeTypes), nrow = 0))
columnNames <-c("Model","Engine",modeTypes)
colnames(checkedModels) <- c("Model","Engine",modeTypes)
checkRow <- checkedModels
#(Model=character(),Engine = character(), modeTypes[[1]]= logical())

for(i in uniqueModels){
  
  try(engineCheck<-(show_engines(i)),silent = TRUE)
  for(j in unique(engineCheck$engine)){
    checkRow<-c(i
                 ,j
                 ,modeTypes[1] %in% engineCheck[engineCheck$engine %in% j,]$mode
                 ,modeTypes[2] %in% engineCheck[engineCheck$engine %in% j,]$mode
                 ,modeTypes[3] %in% engineCheck[engineCheck$engine %in% j,]$mode)
    
  checkedModels[nrow(checkedModels) + 1,] = checkRow    
  }

}
