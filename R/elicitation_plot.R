## Expert Elicitation

expert_elicitation <- function(x) {

  version <- 1
  
    all_sites <- readr::read_csv('data/pollen_meta_v1.0.csv')
  elicitation <- readr::read_csv('data/input/pollen_meta_2014-05-01_compiled.csv')
    pollen_ts <- readr::read_csv(file = paste0('data/sediment_ages_v', version, '.0_varves.csv'))
  
  core_estimates <- function(x) {
    x <- x %>% data.frame
    data.frame(id        = x$datasetID,
               handle    = x$handle,
               depth_inc = unlist(sort(x[c('pre1', 'pre2', 'pre3', 'pre4')])[2]))
  }
  
   elicit <- by_row(elicitation, core_estimates)$`.out` %>% bind_rows %>% filter(depth_inc > 0)
   
   # Now I need to get the original ages, sett age & Bacon ages:
   elicit$sett_age <- all_sites$set.year[match(elicit$handle, all_sites$handle)]
   site_names <- all_downloads %>% map_chr(function(x) x$dataset$site$site.name)
   
   test_ages <- function(x) {
     if(x$depth_inc > 1 & x$id %in% pollen_ts$id) {
      orig   <- all_downloads[[match(x$handle, handles)]]$sample.meta$age[x$depth_inc]
      atype  <- all_downloads[[match(x$handle, handles)]]$sample.meta$age.type[1]
      
      output <- pollen_ts %>% 
        filter(id %in% x$id) %>% 
        filter(row_number() %in% x$depth_inc) %>% 
        select(contains("bacon")) %>% unlist %>% mean(na.rm=TRUE)
      output <- 1950 - output
     } else {
       output <- NA
     }
   }
   
   elicit$bacon_age <- by_row(elicit, test_ages)$`.out` %>% unlist
   
}