## Expert Elicitation

expert_elicitation <- function() {

    all_sites <- readr::read_csv('data/pollen_meta_v1.0.csv')
  elicitation <- readr::read_csv('data/input/pollen_meta_2014-05-01_compiled.csv')
    pollen_ts <- readr::read_csv(file = paste0('data/sediment_ages_v1.0_varves.csv'))
  
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
     
     handles <- sapply(all_downloads, function(x)x$dataset$dataset.meta$collection.handle)
     
     if(x$depth_inc > 1 & x$id %in% pollen_ts$id & x$handle %in% handles) {
       
      orig   <- all_downloads[[match(x$handle, handles)]]$sample.meta$age[x$depth_inc]
      atype  <- all_downloads[[match(x$handle, handles)]]$sample.meta$age.type[1]
      
      output <- pollen_ts %>% 
        filter(id %in% x$id) %>% 
        filter(row_number() %in% x$depth_inc) %>% 
        select(dplyr::contains("bacon")) %>% unlist %>% mean(na.rm=TRUE)
      
      output <- data.frame(bacon = 1950 - output, original = 1950 - orig, type = atype)
     
     } else {
       output <- data.frame(bacon = NA, original = NA, type = NA)
     }
   }
   
   recal <- by_row(elicit, test_ages)$`.out` %>% bind_rows
   
   plot_table <- data.frame(  original = c(recal$original, recal$bacon),
                            settlement = c(elicit$sett_age, elicit$sett_age),
                                 class = c(rep(c('Original Age Model',
                                                 'Bacon Age Model'), each = nrow(elicit))))

   plot_table$class <- factor(plot_table$class, levels = c('Original Age Model',
                                                           'Bacon Age Model'))

   output_plot <- ggplot(plot_table %>% filter(original > 1000 & settlement > 1000), 
                         aes(x = settlement, y = original)) + 
     geom_point() +
     facet_wrap(~class) +
     coord_cartesian(xlim = c(1820, 1920), ylim = c(1200, 2000), expand = FALSE) +
     geom_abline(slope = 1, intercept = 0) +
     geom_smooth(method = 'lm') +
     xlab("Assigned Settlement Age Control (CE)") +
     ylab("Modeled Age (CE)") +
     theme_bw() +
     theme(axis.title.x = element_text(family = 'serif', 
                                       face = 'bold.italic', 
                                       size = 18),
           legend.position = 'none',
           axis.title.y = element_text(family = 'serif', 
                                       face = 'bold.italic', 
                                       size = 18),
           axis.ticks = element_blank(),
           axis.text.x = element_text(family = 'serif', 
                                      face = 'italic', 
                                      size = 14),
           axis.text.y = element_text(family = 'serif', 
                                      face = 'italic', 
                                      size = 14))

   orig_bacon <- ggplot(recal %>% filter(bacon > 100 & original > 1000), 
                        aes(x = original, y = bacon)) + 
     geom_point() +
     geom_abline(slope = 1, intercept = 0) +
     geom_smooth(method = 'lm') +
     xlab("Original Age Model") +
     ylab("Bacon Age Model") +
     theme_bw() +
     theme(axis.title.x = element_text(family = 'serif', 
                                       face = 'bold.italic', 
                                       size = 18),
           legend.position = 'none',
           axis.title.y = element_text(family = 'serif', 
                                       face = 'bold.italic', 
                                       size = 18),
           axis.ticks = element_blank(),
           axis.text.x = element_text(family = 'serif', 
                                      face = 'italic', 
                                      size = 14),
           axis.text.y = element_text(family = 'serif', 
                                      face = 'italic', 
                                      size = 14))
   
   
   return(list(output_plot, orig_bacon))
   
}