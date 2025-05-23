#' Filter rare response variables from the data
#'
#' @details This function allows you to remove response units (OTUs or SNPs or
#' species) from your response data as a preprocessing step. Suitable when the
#' response is a binary outcome.
#' 
#' @param X is a data.frame with rows as sites or individuals or populations and
#' columns as loci or species OTUs.
#' @param lower is the lower threshold value  in which response varialkes are
#' removed from the data.frame.
#' @param higher is the upper threshold value  in which response varialkes are
#' removed from the data.frame.
#' 
#' @returns A filtered tibble.
#' 
#' @examplesIf FALSE
#' X <- filterRareCommon(Responsedata, lower = 0.4, higher = 0.7)
#' 
#' @export

filterRareCommon <- function(X,
                             lower = lower,
                             higher = higher){
  n = ncol(X)
  r = nrow(X)
  Xt <- as.data.frame(t(X))
  Xt$Xsum <- rowSums(Xt[1:n,] )
  FilterNoCommon <- Xt %>%
    dplyr::filter(.data$Xsum < (r * higher))
  FilterNoRare <- FilterNoCommon %>%
    dplyr::filter(.data$Xsum > (r * lower))
  #remove 'new' sort column
  FilterNoRare$Xsum <- NULL
  X <- tibble::as_tibble(t(FilterNoRare))
  X
}

#' Conversion to single column per locus from plink file via LEA functionality
#' 
#' @details Function to import SNP data from a plink format into a format
#' suitable for MrIML predicts (presence/absence of an alelle for each locus).
#' Currently if there is missing data (NAs) it either imputes them as the mode
#' or leaves them. A histogram is also produced of the missing data.
#' 
#' @param pedfile A file location.
#' @param mapfile A file location.
#' 
#' @returns A tibble.
#' 
#' @examplesIf FALSE
#' snps <- readSnpsPed("FILE_NAME.plink.ped", "FILE_NAME.plink.map.map")
#' X <- filterRareCommon(snps, lower = 0.4, higher = 0.7) 
#' 
#' @export 
readSnpsPed <- function(pedfile, mapfile){
  
  if (missing(pedfile))
    stop("missing plink '.ped' file")
  if (missing(mapfile))
    stop("missing plink '.map' file")
  
  # Isolate SNP matrix from .ped file and and append sample IDs as row names
  snpobj <- utils::read.table(
    pedfile,
    row.names = 2,
    na.strings = c("0", "-9"),
    stringsAsFactors = FALSE
  )
  snpobj <- snpobj[-(1:5)]
  
  # Extract locus IDs from .map file, create col names and append to SNP matrix
  lnames <- utils::read.table(mapfile)[,2]
  locnames1 <- paste0(lnames, ".1")
  locnames2 <- paste0(lnames, ".2")
  locnames3 <- c(rbind(locnames1, locnames2))
  colnames(snpobj) <- locnames3
  
  if (any(is.na(snpobj))){
    graphics::hist(
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
        snpobj[i] <- tidyr::replace_na(
          snpobj[i],
          Mode(snpobj[i], na.rm = TRUE)
        ) 
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
  
  data.frame(snpobj, rownames.force = TRUE) %>%
    tibble::as_tibble()
  
}

#' Calculates resistance components from a list of pairwise resistance surfaces
#'
#' @details Outputs a data frame of significant resistance components for each
#' matrix in the target folder. These data can be combined with non-pairwise
#' matrix data.
#'
#' @param foldername A \code{character} this is the location where the
#' resistance surfaces are stored.
#' @param p_val A \code{numeric} this sets the significance threshold for axes
#' in explaining variance in the original resistance matrix based on redundancy
#' analysis. In effect this filters out axes that don't explain variance.
#' @param cl A parallel argument to be passed to [vegan::capscale()] if parallel
#' compute is wanted.
#' 
#' @returns A data frame.
#' 
#' @examplesIf FALSE
#' Y <- resist_components(filename = 'FILE_PATH', p_val = 0.01)
#'
#' @export 

resist_components <- function (foldername = foldername,
                               p_val = p_val,
                               cl = NULL){
  # Require the 'vegan' package to be installed
  if (!requireNamespace("vegan", quietly = TRUE)) {
    message(
      paste0("The 'vegan' package is required for this function. Would you ",
             "like to install it? (yes/no)")
    )
    response <- readline()
    if (tolower(response) == "yes") {
      utils::install.packages("vegan")
    } else {
      stop(
        paste0("The 'vegan' package is needed for this function. Please ",
               "install it to proceed.")
      )
    }
  }
  # Require the 'ape' package to be installed
  if (!requireNamespace("ape", quietly = TRUE)) {
    message(
      paste0("The 'ape' package is required for this function. Would you ",
             "like to install it? (yes/no)")
    )
    response <- readline()
    if (tolower(response) == "yes") {
      utils::install.packages("ape")
    } else {
      stop(
        paste0("The 'ape' package is needed for this function. Please ",
               "install it to proceed.")
      )
    }
  }
  
  files <- list.files(paste(foldername))
  
  n_matrix <- length(files)
  
  #for (i in 1:length(files)){
  final_d <- lapply(
    seq(1, n_matrix),
    function(i) {
    # ! Need to be csv in a folder within your working directory
    data_resist <- utils::read.csv(paste0("./", foldername,'/', files[i]))
    #remove row information. It is important that the matrix is symmetric
    data_resist[1] <- NULL
    
    # Colnames should be names of the population/sites/etc
    siteData<- as.data.frame(names(data_resist)) 
    
    names(siteData) <- c('Site') # For identifiability
    
    #names need to match in each matrix
    data_resist <- (data_resist / max(data_resist))
    
    res <- ape::pcoa(data_resist)
    
    # variance explained by pcoa 1
    l1 <- round(res$values$Relative_eig[1], 2)
    
    # variance explained by pcoa 2 etc
    l2 <- round(res$values$Relative_eig[2], 2) 
    
    n_axes <- ncol(res$vectors)
    
    pcdat <- as.data.frame(res$vectors)
    
    
    ##########Plot##################
    
    PcoA2D_AT <- res$vectors[,1:2]
    # So here simply the Euclidean distance is used, see distances above
    
    PcoA2D_AT <- as.data.frame(PcoA2D_AT)
    
    names(PcoA2D_AT)[1:2] <- c('PCoA1', 'PCoA2')
    
    #plot it
    Tr_PcoA <- ggplot2::ggplot( # Would be good to make this 3D at some point
      PcoA2D_AT,
      ggplot2::aes(x = .data$PCoA1, y = .data$PCoA2, label = siteData$Site)
    ) +
      ggplot2::geom_point(size =2) +
      ggplot2::geom_text(
        col = 'black',
        size = 4,
        check_overlap = TRUE
      ) +
      ggplot2::labs(
        title = paste('PCoA', gsub('.csv','', files[i])),
        y = paste0(
          "PCoA-2 (",
          paste0((l2 * 100), "%"),
          " variance explained)"
        ), 
        x = paste0(
          "PCoA-1 (",
          paste0((l1 * 100), "%"),
          " variance explained)"
        )
      ) +
      ggplot2::theme_bw()
    
    print(Tr_PcoA)
    
    #------------------------------------------ 
    #Select only significant axes
    #------------------------------------------   
    
    sigPCs <- NULL
    
    for(j in 1:n_axes){
      
      vec <- pcdat[j]
      
      axisName <- names(vec)
      
      names(vec) <- c('pc_axis')
      
      ## Basic dbRDA Analysis
      vare.cap <- vegan::capscale(
        data_resist ~ vec$pc_axis,
        parallel = cl
      )
      
      sig <- stats::anova(vare.cap) #999 permutations
      
      pval <- sig$`Pr(>F)`
      
      sigPCs[[j]] <- c(axisName, pval[1])
      
    }
    
    sigPC <- as.data.frame(do.call(rbind, sigPCs))
    
    names(sigPC) <-  c('Axis', 'P-value')
    
    sigPC$`P-value` <- as.numeric(as.character(sigPC$`P-value`))
    
    #select significant pcs
    threholdScore <- sigPC %>%
      dplyr::filter(.data$`P-value`<= p_val)
    
    #------------------------------------------   
    
    #select informative axes
    ReducedAxis <- pcdat %>%
      subset(select = threholdScore$Axis)
    
    # add site data
    ReducedAxisDF <- cbind(siteData, ReducedAxis)
    rownames(ReducedAxisDF) <- ReducedAxisDF$Site
    ReducedAxisDF$Site <-NULL
    
    #add resistance surface names
    names(ReducedAxisDF) <- paste(
      gsub('.csv','', files[i]),
      names(ReducedAxisDF),
      sep='_'
    )
    
    ReducedAxisDF
  })
  #save as a data frame
  as.data.frame(do.call(cbind, final_d))
}
