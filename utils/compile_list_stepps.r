compile_list_neotoma <- function (object, list.name, cf = TRUE, type = TRUE) 
{
  if (!class(object) %in% c("list", "matrix", "data.frame")) {
    stop("Data object must be a pollen object returned by function get_download or a matrix or data.frame")
  }
  #   data(pollen.equiv)
  pollen.equiv <- read.csv("data/pollen/pollen.equiv.csv", stringsAsFactors=F, sep=',', row.names=NULL)
  avail.lists <- c("P25", "WS64", "WhitmoreFull", "WhitmoreSmall", "Stepps")
  
  if (cf == FALSE) 
    list.name <- list.name[is.na(pollen.equiv$cf)]
  if (type == FALSE) 
    list.name <- list.name[is.na(pollen.equiv$type)]
  use.list <- which(avail.lists %in% list.name)
  if (class(object) == "list") {
    
    #     colnames(object$counts)[which(substr(colnames(object$counts), 1,3) == 'Iso')] = 'Isoetes'
    
    if (!all(colnames(object$counts) %in% pollen.equiv$taxon)){
      print((!all(colnames(object$counts) %in% pollen.equiv$taxon)))
      print("Some taxa are missing from the conversion table!")
      print(colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv$taxon)])
    }
    
    used.taxa <- pollen.equiv[match(colnames(object$counts), 
                                    pollen.equiv$taxon), ]
    agg.list <- as.vector(used.taxa[, use.list + 2])
    agg.list[is.na(agg.list)] <- "Other"
    compressed.list <- aggregate(t(object$counts), by = list(agg.list), 
                                 sum, na.rm = TRUE)
    compressed.cols <- compressed.list[, 1]
    compressed.list <- t(compressed.list[, -1])
    colnames(compressed.list) <- compressed.cols
    new.list <- object$taxon.list
    new.list$compressed <- NA
    new.list$compressed <- as.character(pollen.equiv[match(new.list$TaxonName, 
                                                           pollen.equiv$taxon), use.list + 2])
    new.list$compressed[is.na(new.list$compressed) & new.list$TaxonName %in% 
                          colnames(object$counts)] <- "Other"
    output <- list(metadata = object$metadata, sample.meta = object$sample.meta, 
                   taxon.list = new.list, counts = compressed.list, 
                   lab.data = object$lab.data)
  }
  if (class(object) %in% c("matrix", "data.frame")) {
    if (!all(colnames(object) %in% pollen.equiv$taxon)){
      print("Some taxa are missing from the conversion table!")
      print(colnames(object)[!(colnames(object) %in% pollen.equiv$taxon)])
    }
    used.taxa <- pollen.equiv[match(colnames(object), pollen.equiv$taxon), 
                              ]
    agg.list <- as.vector(used.taxa[, use.list + 2])
    agg.list[is.na(agg.list)] <- "Other"
    compressed.list <- aggregate(t(object), by = list(agg.list), 
                                 sum, na.rm = TRUE)
    compressed.cols <- compressed.list[, 1]
    compressed.list <- t(compressed.list[, -1])
    colnames(compressed.list) <- compressed.cols
    output <- compressed.list
  }
  return(output)
}


compile_list_stepps <- function(object, list.name='must_have', pollen.equiv.stepps=pollen.equiv.stepps, cf = TRUE, type = TRUE){

  if(!class(object) %in% c('list', 'matrix', 'data.frame')){
    stop('Data object must be a pollen object returned by function get_download or a matrix or data.frame')
  }
  
  #   data(pollen.equiv.stepps)
  
  avail.lists <- c('all', 'must_have')
  
  use.list <- which(avail.lists %in% list.name)
  
  taxa  = sort(unique(pollen.equiv.stepps[!is.na(pollen.equiv.stepps[,use.list+1]),use.list+1]))
  ntaxa = length(taxa)
  
  if(class(object) == 'list'){
    if (!all(colnames(object$counts) %in% pollen.equiv.stepps$taxon)){
      print("Some taxa are missing from the conversion table!")
      print(colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv.stepps$taxon)])
    }
    used.taxa <- pollen.equiv.stepps[match(colnames(object$counts), pollen.equiv.stepps$taxon),]
    agg.list <- as.vector(used.taxa[,use.list + 1])
    #     agg.list[is.na(agg.list)] <- 'barf'
    
    compressed.list <- aggregate(t(object$counts), by = list(agg.list), sum, na.rm=TRUE)
    
    compressed.cols <- compressed.list[,1]
    
    compressed.list <- t(compressed.list[,-1])
    colnames(compressed.list) <- compressed.cols
    
    # add back the taxa that have no counts
    zero_taxa = taxa[!(taxa %in% colnames(compressed.list))]
    add_back = matrix(0, nrow=nrow(compressed.list), ncol=length(zero_taxa))
    colnames(add_back) = zero_taxa
    
    compressed.list = cbind(compressed.list, add_back)
    compressed.list = compressed.list[, sort(colnames(compressed.list))]
    
    counts_full = data.frame(matrix(0, nrow=nrow(object$counts), ncol=ntaxa))
    colnames(counts_full) = taxa
    idx = match(colnames(compressed.list), colnames(counts_full))
    
    for (j in 1:length(idx)){
      if (!is.na(idx[j])){  
        counts_full[,idx[j]] = compressed.list[,j]
      } 
    }
    
    #  We want to make a taxon list like the one returned in get_downloads:
    new.list <- object$taxon.list
    new.list$compressed <- NA
    
    new.list$compressed <- as.character(pollen.equiv.stepps[match(new.list$compressed, pollen.equiv.stepps$taxon),use.list + 1])  
    new.list$compressed[is.na(new.list$compressed) & new.list$compressed %in% colnames(object$counts)] <- 'Other'
    
    #  Returns a data.frame with taxa in the columns and samples in the rows.
    output <- list(metadata = object$metadata,
                   sample.meta = object$sample.meta,
                   taxon.list = new.list, 
                   counts = compressed.list,
                   lab.data = object$lab.data)
  }
  if(class(object) %in% c('matrix', 'data.frame')){
    if (!all(colnames(object) %in% pollen.equiv.stepps$taxon)){
      print("Some taxa are missing from the conversion table!")
      print(colnames(object)[!(colnames(object) %in% pollen.equiv.stepps$taxon)])
    }
    used.taxa <- pollen.equiv.stepps[match(colnames(object), pollen.equiv.stepps$taxon),]
    agg.list <- as.vector(used.taxa[,use.list + 1])
    #     agg.list[is.na(agg.list)] <- 'barf'
    
    compressed.list <- aggregate(t(object), by = list(agg.list), sum, na.rm=TRUE)
    
    compressed.cols <- compressed.list[,1]
    
    compressed.list <- t(compressed.list[,-1])
    colnames(compressed.list) <- compressed.cols
    
    # add back the taxa that have no counts
    zero_taxa = taxa[!(taxa %in% colnames(compressed.list))]
    add_back = matrix(0, nrow=nrow(compressed.list), ncol=length(zero_taxa))
    colnames(add_back) = zero_taxa
    
    compressed.list = cbind(compressed.list, add_back)
    compressed.list = compressed.list[, sort(colnames(compressed.list))]
    
    counts_full = data.frame(matrix(0, nrow=nrow(object), ncol=ntaxa))
    colnames(counts_full) = taxa
    if (nrow(object)==1){
      idx = match(names(compressed.list), colnames(counts_full))
      for (j in 1:length(idx)){
        if (!is.na(idx[j])){  
          counts_full[,idx[j]] = compressed.list[j]
        } 
      }
    } else{
      idx = match(colnames(compressed.list), colnames(counts_full))
      for (j in 1:length(idx)){
        if (!is.na(idx[j])){  
          counts_full[,idx[j]] = compressed.list[,j]
        } 
      }
    }
    
    output <- counts_full
  }
  
  return(output)
  
}


compile_taxa_stepps <- function (object, list.name='must_have', alt.table = pollen.equiv.stepps, cf = TRUE, 
                                 type = TRUE) 
{
  if (!inherits(object, c("matrix", "data.frame", "download", 
                          "download_list"))) {
    stop(paste("Data object must be a pollen object returned by", 
               "function get_download or a matrix or data frame"))
  }
  if (!is.null(alt.table)) {
    if (!inherits(alt.table, c("matrix", "data.frame"))) {
      stop("The alt.table must be either a matrix or a data frame.")
    }
    pollen.equiv <- alt.table
    avail.lists <- colnames(pollen.equiv)
    if (!list.name %in% avail.lists) {
      stop("The list name is not included in your alt.table.")
    }
    if (!"taxon" %in% (avail.lists)) {
      stop("The alt.table must contain a column titled taxon.")
    }
    use.list <- which(avail.lists %in% list.name)
  }
  else {
    pollen.equiv <- NULL
    data(pollen.equiv, envir = environment())
    avail.lists <- c("P25", "WS64", "WhitmoreFull", "WhitmoreSmall")
    use.list <- which(avail.lists %in% list.name) + 2
  }
  if (cf == FALSE) 
    list.name <- list.name[is.na(pollen.equiv$cf)]
  if (type == FALSE) 
    list.name <- list.name[is.na(pollen.equiv$type)]
  if (inherits(object, c("download", "download_list"))) {
    aggregate.counts <- function(x) {
      taxon.matches <- match(tolower(colnames(x$counts)), tolower(pollen.equiv$taxon))
      if (any(is.na(taxon.matches))) {
        missed.samples <- colnames(x$counts)[is.na(taxon.matches)]
      }
      used.taxa <- pollen.equiv[taxon.matches, ]
      agg.list <- as.vector(used.taxa[, use.list])
      agg.list[is.na(agg.list)] <- "Other"
      compressed.list <- aggregate(t(x$counts), by = list(agg.list), 
                                   sum, na.rm = TRUE)
      compressed.cols <- compressed.list[, 1]
      compressed.list <- t(compressed.list[, -1])
      colnames(compressed.list) <- compressed.cols

      new.list <- x$taxon.list
      new.list$compressed <- NA
      new.list$compressed <- as.character(pollen.equiv[match(new.list$taxon.name, 
                                                             pollen.equiv$taxon), use.list])
      new.list$compressed[is.na(new.list$compressed) & 
                            new.list$taxon.name %in% colnames(x$counts)] <- "Other"
      new.list <- new.list[match(new.list$taxon.name, x$taxon.list$taxon.name), 
                           ]
      
      output <- list(dataset = x$dataset, sample.meta = x$sample.meta, 
                     taxon.list = new.list, counts = compressed.list, 
                     full.counts = x$counts, lab.data = x$lab.data, 
                     chronologies = x$chronologies)
      missed <- as.character(unique(new.list$taxon.name[which(new.list$compressed == 
                                                                "Other")]))
      if (length(missed)>0){
        warning(paste0("\nThe following taxa could not be found in the existing ", 
                       "conversion table:\n", paste(missed, sep = "\n")))
      }
      class(output) <- c("download", "list")
      output
    }
    if (inherits(object, "download_list")) {
      output <- lapply(object, FUN = aggregate.counts)
      class(output) <- c("download_list", "list")
    }
    else {
      output <- aggregate.counts(object)
      class(output) <- c("download", "list")
    }
  }
  if (inherits(object, c("matrix", "data.frame"))) {
    taxon.matches <- match(colnames(object), pollen.equiv$taxon)
    if (any(is.na(taxon.matches))) {
      missed.samples <- colnames(object)[is.na(taxon.matches)]
      warning(paste0("\nThe following taxa could not be found in the existing ", 
                     "conversion table:\n", paste(missed.samples, 
                                                  sep = "\n")))
    }
    used.taxa <- pollen.equiv[taxon.matches, ]
    agg.list <- as.vector(used.taxa[, use.list])
    agg.list[is.na(agg.list)] <- "Other"
    compressed.list <- aggregate(t(object), by = list(agg.list), 
                                 sum, na.rm = TRUE)
    compressed.cols <- compressed.list[, 1]
    compressed.list <- t(compressed.list[, -1])
    colnames(compressed.list) <- compressed.cols
    output <- compressed.list
  }
  output
}