compile_list_neotoma <- function (object, list.name, pollen.equiv, cf = TRUE, type = TRUE) 
{
  if (!inherits(object, c('matrix', 'data.frame', 'download', 'download_list'))) {
    stop(paste('Data object must be a pollen object returned by',
               'function get_download or a matrix or data frame'))
  }
  #   data(pollen.equiv)
  # pollen.equiv <- read.csv("pollen.equiv.csv", stringsAsFactors=F, sep=',', row.names=NULL)
  avail.lists <- c("P25", "WS64", "WhitmoreFull", "WhitmoreSmall", "Stepps")
  
  if (cf == FALSE) 
    list.name <- list.name[is.na(pollen.equiv$cf)]
  if (type == FALSE) 
    list.name <- list.name[is.na(pollen.equiv$type)]
  use.list <- which(avail.lists %in% list.name)
  
  taxa  = sort(unique(pollen.equiv[!is.na(pollen.equiv[,use.list+2]),use.list+2]))
  ntaxa = length(taxa)
  
  if (inherits(object, c("download","download_list"))) {
    object=object
    
    #     colnames(object$counts)[which(substr(colnames(object$counts), 1,3) == 'Iso')] = 'Isoetes'
    
    if (!all(colnames(object$counts) %in% pollen.equiv$taxon)){
      print((!all(colnames(object$counts) %in% pollen.equiv$taxon)))
      print("Some taxa are missing from the conversion table!")
      print(colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv$taxon)])
      # miss = colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv$taxon)]
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
    
    if (any(is.na(idx))){
      if (sum(is.na(idx)) > 1){ 
        counts_full$Other = rowSums(compressed.list[,is.na(idx)])
      } else {
        counts_full$Other = compressed.list[,is.na(idx)]
      }
    }
    
    new.list <- object$taxon.list
    new.list$compressed <- NA
    new.list$compressed <- as.character(pollen.equiv[match(new.list$taxon.name, 
                                                           pollen.equiv$taxon), use.list + 2])
    new.list$compressed[is.na(new.list$compressed) & new.list$taxon.name %in% 
                          colnames(object$counts)] <- "Other"
    output <- list(dataset = object$dataset, sample.meta = object$sample.meta, 
                   taxon.list = new.list, counts = counts_full, 
                   lab.data = object$lab.data,
                   chronologies = object$chronologies)
  }
  if (inherits(object, c("matrix", "data.frame"))) {
    taxon.matches <- match(colnames(object), pollen.equiv$taxon)
    if (any(is.na(taxon.matches))){
      missed.samples <- colnames(object)[is.na(taxon.matches)]
      warning(paste0('\nThe following taxa could not be found in the existing ',
                     'conversion table:\n', paste(missed.samples, sep = '\n')))
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

# compile_list_neotoma <- function (object, list.name, cf = TRUE, type = TRUE) 
# {
#   if (!inherits(object, c('matrix', 'data.frame', 'download', 'download_list'))) {
#     stop(paste('Data object must be a pollen object returned by',
#                'function get_download or a matrix or data frame'))
#   }
#   #   data(pollen.equiv)
#   pollen.equiv <- read.csv("pollen.equiv.csv", stringsAsFactors=F, sep=',', row.names=NULL)
#   avail.lists <- c("P25", "WS64", "WhitmoreFull", "WhitmoreSmall", "Stepps")
#   
#   missed=NA
#   
#   if (cf == FALSE) 
#     list.name <- list.name[is.na(pollen.equiv$cf)]
#   if (type == FALSE) 
#     list.name <- list.name[is.na(pollen.equiv$type)]
#   use.list <- which(avail.lists %in% list.name)
#   
#   taxa  = sort(unique(pollen.equiv[!is.na(pollen.equiv[,use.list+2]),use.list+2]))
#   ntaxa = length(taxa)
#   
#   if (inherits(object, c("download","download_list"))) {
#     object=object
#     
#     #     colnames(object$counts)[which(substr(colnames(object$counts), 1,3) == 'Iso')] = 'Isoetes'
#     
#     if (!all(colnames(object$counts) %in% pollen.equiv$taxon)){
#       print((!all(colnames(object$counts) %in% pollen.equiv$taxon)))
#       print("Some taxa are missing from the conversion table!")
#       print(colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv$taxon)])
#       missed = colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv$taxon)]
#     }
#     
#     used.taxa <- pollen.equiv[match(colnames(object$counts), 
#                                     pollen.equiv$taxon), ]
#     agg.list <- as.vector(used.taxa[, use.list + 2])
#     agg.list[is.na(agg.list)] <- "Other"
#     compressed.list <- aggregate(t(object$counts), by = list(agg.list), 
#                                  sum, na.rm = TRUE)
#     compressed.cols <- compressed.list[, 1]
#     compressed.list <- t(compressed.list[, -1])
#     colnames(compressed.list) <- compressed.cols
#     
#     # add back the taxa that have no counts
#     zero_taxa = taxa[!(taxa %in% colnames(compressed.list))]
#     add_back = matrix(0, nrow=nrow(compressed.list), ncol=length(zero_taxa))
#     colnames(add_back) = zero_taxa
#     
#     compressed.list = cbind(compressed.list, add_back)
#     compressed.list = compressed.list[, sort(colnames(compressed.list))]
#     
#     counts_full = data.frame(matrix(0, nrow=nrow(object$counts), ncol=ntaxa))
#     colnames(counts_full) = taxa
#     idx = match(colnames(compressed.list), colnames(counts_full))
#     
#     for (j in 1:length(idx)){
#       if (!is.na(idx[j])){  
#         counts_full[,idx[j]] = compressed.list[,j]
#       } 
#     }
#     
#     if (any(is.na(idx))){
#       if (sum(is.na(idx)) > 1){ 
#         counts_full$Other = rowSums(compressed.list[,is.na(idx)])
#       } else {
#         counts_full$Other = compressed.list[,is.na(idx)]
#       }
#     }
#     
#     new.list <- object$taxon.list
#     new.list$compressed <- NA
#     new.list$compressed <- as.character(pollen.equiv[match(new.list$taxon.name, 
#                                                            pollen.equiv$taxon), use.list + 2])
#     new.list$compressed[is.na(new.list$compressed) & new.list$taxon.name %in% 
#                           colnames(object$counts)] <- "Other"
#     output <- list(dataset = object$dataset, sample.meta = object$sample.meta, 
#                    taxon.list = new.list, counts = counts_full, 
#                    lab.data = object$lab.data,
#                    chronologies = object$chronologies)
#   }
#   if (inherits(object, c("matrix", "data.frame"))) {
#     taxon.matches <- match(colnames(object), pollen.equiv$taxon)
#     if (any(is.na(taxon.matches))){
#       missed.samples <- colnames(object)[is.na(taxon.matches)]
#       warning(paste0('\nThe following taxa could not be found in the existing ',
#                      'conversion table:\n', paste(missed.samples, sep = '\n')))
#       missed = missed.samples
#     }
#     
#     used.taxa <- pollen.equiv[match(colnames(object), pollen.equiv$taxon), 
#                               ]
#     agg.list <- as.vector(used.taxa[, use.list + 2])
#     agg.list[is.na(agg.list)] <- "Other"
#     compressed.list <- aggregate(t(object), by = list(agg.list), 
#                                  sum, na.rm = TRUE)
#     compressed.cols <- compressed.list[, 1]
#     compressed.list <- t(compressed.list[, -1])
#     colnames(compressed.list) <- compressed.cols
#     output <- compressed.list
#   }
#   return(list(output, missed))
# }

compile_list_stepps <- function(object, list.name='must_have', pollen.equiv.stepps=pollen.equiv.stepps, cf = TRUE, type = TRUE){
  
  if (!inherits(object, c('matrix', 'data.frame', 'download', 'download_list', 'list'))) {
    stop(paste('Data object must be a pollen object returned by',
               'function get_download or a matrix or data frame'))
  }
  
  #   data(pollen.equiv.stepps)
  
  avail.lists <- c('all', 'must_have', 'kujawa')
  
  use.list <- which(avail.lists %in% list.name)
  
  taxa  = sort(unique(pollen.equiv.stepps[!is.na(pollen.equiv.stepps[,use.list+2]),use.list+2]))
  ntaxa = length(taxa)
  
  if (inherits(object, c("download","download_list", "list"))) {
    if (!all(colnames(object$counts) %in% pollen.equiv.stepps$taxon)){
      print("Some taxa are missing from the conversion table!")
      print(colnames(object$counts)[!(colnames(object$counts) %in% pollen.equiv.stepps$taxon)])
    }
    used.taxa <- pollen.equiv.stepps[match(colnames(object$counts), pollen.equiv.stepps$taxon),]
    agg.list <- as.vector(used.taxa[,use.list + 2])
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
    output <- list(dataset = object$dataset,
                   sample.meta = object$sample.meta,
                   taxon.list = new.list, 
                   counts = compressed.list,
                   lab.data = object$lab.data,
                   chronologies=object$chronologies)
  }
  if (inherits(object, c("matrix", "data.frame"))) {
    taxon.matches <- match(colnames(object), pollen.equiv.stepps$taxon)
    if (any(is.na(taxon.matches))){
      missed.samples <- colnames(object)[is.na(taxon.matches)]
      warning(paste0('\nThe following taxa could not be found in the existing ',
                     'conversion table:\n', paste(missed.samples, sep = '\n')))
    }
    used.taxa <- pollen.equiv.stepps[match(colnames(object), pollen.equiv.stepps$taxon),]
    agg.list <- as.vector(used.taxa[,use.list + 2])
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

