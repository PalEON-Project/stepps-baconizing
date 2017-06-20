#' @title Pull all North American Pollen sites from Neotoma
#' @param version The version number of the run.
#' @description This code takes the current version number of the paper, and then searches for all sites with pollen data within North America, returning the \code{download} objects associated with those sites.
#' @return A long \code{download_list} for all pollen sites in North America.
#' 
north_american_cores <- function(version) {
  
  gpid_table <- neotoma::get_table("GeoPoliticalUnits") %>% 
    filter(GeoPoliticalName %in% c('Canada', 'United States', 'Mexico')) %>% 
    select(GeoPoliticalID) %>% 
    unlist %>% 
    map(~ neotoma::get_dataset(gpid = .x, datasettype = 'pollen'))
  
  na_datasets <- neotoma::bind(neotoma::bind(gpid_table[[1]], gpid_table[[2]]), gpid_table[[3]])
  
  if(paste0('all_downloads_v', version, '.rds') %in% list.files('data/output')){
    
    all_downloads <- readRDS(paste0('data/output/all_downloads_v', version, '.rds'))
    
  } else{
    
    all_downloads <- get_download(na_datasets, verbose = FALSE)
    
    saveRDS(all_downloads, file = paste0('data/output/all_downloads_v', version, '.rds'))
  }
  if(paste0('all_geochron_v', version, '.rds') %in% list.files('data/output')){
    
    all_geochron <- readRDS(paste0('data/output/all_geochron_v', version, '.rds'))
    
  } else{
    all_sites <- all_downloads %>% get_site
    
    all_geochron <- get_geochron(all_sites, verbose = TRUE)
    
    saveRDS(all_geochron, file = paste0('data/output/all_geochron_v', version, '.rds'))
  
  }
  
  return(all_downloads)
}