compare_blois <- function() {
  
  blois_depths <- read.csv('data/input/blois_files/2011-01-06-all.matched.lp.sites.csv')
  
  blois_depths[blois_depths$Handle == "WOODLAKE",]$picea_decline_depth <- 1100
  
  bacon_files <- list.files('data/input/stepps_out', full.names = TRUE)
  
  # This is where I've put the output files:
  pull_comp <- function(i, bacon_files, blois_depths) {
  
    match_file <- grep(blois_depths$Handle[i], bacon_files)
    
    if (length(match_file == 1)) {
      bacon_file <- read.csv(bacon_files[match_file])
      
      picea_blois <- blois_depths %>% 
        select(Handle, 
               picea_decline_idw_2d_age_mean,
               picea_decline_idw_2d_age_older,
               picea_decline_idw_2d_age_younger,
               picea_decline_depth) %>% 
        filter(Handle == blois_depths$Handle[i])
      
      if (!is.na(picea_blois$picea_decline_depth)) {
        
        picea_row <- bacon_file %>% filter(depths == picea_blois$picea_decline_depth) %>% unlist
        picea_bacon <- data.frame(mean    = mean(picea_row),
                                  older   = quantile(x = picea_row, 0.95),
                                  younger = quantile(x = picea_row, 0.05))
        
      } else {
        picea_bacon <- data.frame(mean = NA, older = NA, younger = NA)
      }
      
      alnus_blois <- blois_depths %>% 
        select(Handle, 
               alnus_decline_idw_2d_age_mean,
               alnus_decline_idw_2d_age_older,
               alnus_decline_idw_2d_age_younger,
               alnus_decline_depth) %>% 
        filter(Handle == blois_depths$Handle[i])

      if (!is.na(alnus_blois$alnus_decline_depth)) {
        
        alnus_row <- bacon_file %>% 
          filter(depths == alnus_blois$alnus_decline_depth) %>% unlist
        alnus_bacon <- data.frame(mean    = mean(alnus_row),
                                  older   = quantile(x = alnus_row, 0.95),
                                  younger = quantile(x = alnus_row, 0.05))
        
      } else {
        alnus_bacon <- data.frame(mean = NA, older = NA, younger = NA)
      }
      
      quercus_blois <- blois_depths %>% 
        select(Handle, 
               quercus_rise_idw_2d_age_mean,
               quercus_rise_idw_2d_age_older,
               quercus_rise_idw_2d_age_younger,
               quercus_rise_depth) %>% 
        filter(Handle == blois_depths$Handle[i])
      
      if (!is.na(quercus_blois$quercus_rise_depth)) {
        
        quercus_row <- bacon_file %>% 
          filter(depths == quercus_blois$quercus_rise_depth) %>% unlist
        
        quercus_bacon <- data.frame(mean    = mean(quercus_row),
                                    older   = quantile(x = quercus_row, 0.95),
                                    younger = quantile(x = quercus_row, 0.05))
        
      } else {
        quercus_bacon <- data.frame(mean = NA, older = NA, younger = NA)
      }
      
      long_mean <- data.frame(handle = as.character(blois_depths$Handle[i]),
                              means_blois = c(picea_blois$picea_decline_idw_2d_age_mean,
                                              alnus_blois$alnus_decline_idw_2d_age_mean,
                                              quercus_blois$quercus_rise_idw_2d_age_mean),
                              means_bacon = c(picea_bacon$mean,
                                              alnus_bacon$mean,
                                              quercus_bacon$mean),
                              younger_bl  = c(picea_blois$picea_decline_idw_2d_age_younger,
                                              alnus_blois$alnus_decline_idw_2d_age_younger,
                                              quercus_blois$quercus_rise_idw_2d_age_younger),
                              younger_ba  = c(picea_bacon$younger,
                                              alnus_bacon$younger,
                                              quercus_bacon$younger),
                              older_bl    = c(picea_blois$picea_decline_idw_2d_age_older,
                                              alnus_blois$alnus_decline_idw_2d_age_older,
                                              quercus_blois$quercus_rise_idw_2d_age_older),
                              older_ba    = c(picea_bacon$older,
                                              alnus_bacon$older,
                                              quercus_bacon$older),
                              taxon  = c("Picea", "Alnus", "Quercus"))
      
    } else {
      long_mean <- data.frame(handle = as.character(blois_depths$Handle[i]),
                              means  = NA,
                              taxon  = NA,
                              source = NA)
    }
    
    return(long_mean)
  }

  # This takes time:
  if (!"comp_depths.rds" %in% list.files('data/output')) {
    comp_depths <- bind_rows(lapply(1:nrow(blois_depths), 
                                    function(x) pull_comp(x, bacon_files, blois_depths)))
    saveRDS(comp_depths, file = "data/output/comp_depths.rds")
  } else {
    comp_depths <- readRDS("data/output/comp_depths.rds")
  }
  
  # This is data cleaning:
  comp_depths <- comp_depths %>% filter(!(taxon == "Alnus" | is.na(taxon)))
  
  out_plot <- ggplot(comp_depths, aes(x = means_blois, y = means_bacon)) +
    geom_point() +
    geom_errorbar(aes(ymin = younger_ba, ymax = older_ba), width = 0.25) +
    geom_errorbarh(aes(xmin = younger_bl, xmax = older_bl)) +
    facet_wrap(~taxon) +
    coord_equal()
  
  comp_coords <- blois_depths[match(comp_depths$handle, blois_depths$Handle),c("Latitude", "Longitude")]
  
  comp_space <- data.frame(comp_depths, comp_coords) %>% 
    filter(!(is.na(means_blois) & is.na(means_bacon)))
  
  ggplot(comp_space, aes(x = Latitude, y = Longitude)) +
    geom_point() +
    facet_wrap(~taxon)
  
  return(list(out_plot, comp_depths))
}
