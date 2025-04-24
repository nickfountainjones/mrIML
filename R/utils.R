#' Filter rare response variables from the data
#'
#' @details This function allows you to remove response units (OTUs or SNPs or
#' species) from your response data as a preprocessing step. Suitable when the
#' response is a binary outcome.
#' @param X is a data.frame with rows as sites or individuals or populations and
#' columns as loci or species OTUs.
#' @param lower is the lower threshold value  in which response varialkes are
#' removed from the data.frame.
#' @param higher is the upper threshold value  in which response varialkes are
#' removed from the data.frame.
#' @examples
#' \dontrun{ 
#'   X <- filterRareCommon(Responsedata, lower = 0.4, higher = 0.7)
#' }
#' @export

filterRareCommon <- function(X,
                             lower = lower,
                             higher = higher){
  n = ncol(X)
  r = nrow(X)
  Xt <- as.data.frame(t(X))
  Xt$Xsum <- rowSums(Xt[1:n,] )
  FilterNoCommon <- subset(Xt, Xsum < (r * higher)) 
  FilterNoRare <- subset(FilterNoCommon , Xsum > (r * lower))
  #remove 'new' sort column
  FilterNoRare$Xsum <- NULL
  X <- as.data.frame(t(FilterNoRare))
  X
}

#' Conversion to single column per locus from plink file via LEA functionality
#' 
#' @details Function to import SNP data from a plink format into a format
#' suitable for MrIML predicts (presence/absence of an alelle for each locus).
#' Currently if there is missing data (NAs) it either imputes them as the mode
#' or leaves them. A histogram is also produced of the missing data.
#' 
#' @param pedfile
#' @param mapfile
#' 
#' @examples
#' \dontrun{
#'  snps <- readSnpsPed("bobcat.plink.ped", "bobcat.plink.map.map")
#'  X <- filterRareCommon(snps, lower = 0.4, higher = 0.7) 
#' }
#' @export 
readSnpsPed <- function(pedfile, mapfile){
  
  if (missing(pedfile))
    stop("missing plink '.ped' file")
  if (missing(mapfile))
    stop("missing plink '.map' file")
  
  # Isolate SNP matrix from .ped file and and append sample IDs as row names
  snpobj <- read.table(
    pedfile,
    row.names = 2,
    na.strings = c("0", "-9"),
    stringsAsFactors = FALSE
  )
  snpobj <- snpobj[-(1:5)]
  
  # Extract locus IDs from .map file, create col names and append to SNP matrix
  lnames <- read.table(mapfile)[,2]
  locnames1 <- paste0(lnames, ".1")
  locnames2 <- paste0(lnames, ".2")
  locnames3 <- c(rbind(locnames1, locnames2))
  colnames(snpobj) <- locnames3
  
  if (any(is.na(snpobj))){
    hist(
      colSums(is.na(as.matrix(snpobj))),
      main = "Missing genotypes per SNP",
      xlab = "No. missing genotpyes"
    )
    Sys.sleep(1)
    impute <- readline(
      paste("This dataset contains NAs. Please select an imputation method",
            "(type a number): 0. none, 1. mode ")
    )
    
    if (impute == "1"){
      # Function for finding the major allele for each column
      Mode <- function(x, na.rm = FALSE) {
        if (na.rm) x = x[!is.na(x)]
        ux <- unique(x)
        
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      for(i in 1:ncol(snpobj)){
        # Replace NAs with the major allele at a given locus
        snpobj[i] <- na.replace(snpobj[i], Mode(snpobj[i], na.rm = TRUE)) 
        # Re-code the major allele as "0"
        snpobj[i] <- replace(
          snpobj[i],
          snpobj[i] == Mode(snpobj[i], na.rm = TRUE),
          "0"
        )
        # Re-code the minor allele as "1"
        snpobj[i] <- replace(
          snpobj[i],
          snpobj[i] %in% c("A", "G", "T", "C"), 
          "1"
        )
      }
    }
  } else {
    for(i in 1:ncol(snpobj)){
      # Re-code the major allele as "0"
      snpobj[i] <- replace(
        snpobj[i],
        snpobj[i] == Mode(snpobj[i], na.rm = TRUE),
        "0"
      )
      # Re-code the minor allele as "1"
      snpobj[i] <- replace(
        snpobj[i], 
        snpobj[i] %in% c("A", "G", "T", "C"),
        "1"
      )
    }
  }
  
  data.matrix(snpobj, rownames.force = TRUE)
  
}