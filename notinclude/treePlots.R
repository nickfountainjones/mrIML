library(ape)

miscTree <- rtree(n = 3)

fromNodes <- as.integer(t(c(5,6,7,7,6,5)))
tooNodes <- as.integer(t(c(6,7,1,2,3,4)))
Nnode <- as.integer(3)
tip.label <- c("Model", "Data", "Fits","Dummy")

#edge.length <- rep(5,length(tooNodes))
edge.length <- c(0,0,1,1,1,1)
edge <- matrix(c(fromNodes, tooNodes), ncol = 2)

manTree <- miscTree

manTree$edge <- edge
manTree$tip.label <- tip.label
manTree$Nnode <- Nnode
manTree$edge.length <- edge.length
plot(manTree)