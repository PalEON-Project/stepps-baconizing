compare_lead <- function() {
  geochron_tables <- readRDS('data/output/all_geochron_tables.rds')
  
  widen <- function(x) {
    data.frame(x$dataset$site.data,
               x$geochron,
               row.names = NULL)
  }
  
  wide_table <- geochron_tables %>% 
    purrr::map(widen) %>% bind_rows
  
  leads <- wide_table %>% filter(geo.chron.type %in% "Lead-210") %>% 
    select(site.name, lat, long, sample.id, depth, age, e.older, e.young)
  
  # There are some ages with improperly named age types.
  leads$age[!is.na(leads$age) & leads$age > 500] <- 1950 - 
    leads$age[!is.na(leads$age) & leads$age > 500]
  
  site_table <- read.csv('data/pollen_meta_v7.csv', stringsAsFactors = FALSE)
  
  leads$handles <- (site_table$handle[match(leads$site.name, site_table$name)])
  
  leads$age[leads$age.type %in% "Calendar years AD/BC" & leads$age > 500] <- 1950 - 
    leads$age[leads$age.type %in% "Calendar years AD/BC" & leads$age > 500]
  
  pull_age <- function(x) {
    
    handle <- x$handles
    depth  <- x$depth
    
    # Get the age estimate from the Bacon model:
    if(is.na(handle) | is.na(depth) | !file.exists(paste0('Cores/', handle, '/', handle, '.csv'))){
      return(data.frame(bacon_age = as.numeric(NA), bacon_error = as.numeric(NA)))
    }
    
    core <- read.csv(paste0('Cores/', handle, '/', handle, '.csv'))
    
    out <- data.frame(bacon_age   = 1950 - approx(x = core$depth, core$age, xout = depth)$y,
                      bacon_error = approx(x = core$depth, core$error, xout = depth)$y)
    
    return(out)
    
  }
  
  bacon_bound <- leads %>% purrrlyr::by_row(pull_age, .collate = 'cols')
  
  lead_plot <- ggplot(bacon_bound) + 
    geom_abline(slope = 1, alpha = 0.4) +
    geom_errorbar(aes(x = 1950 - age,
                      ymin = bacon_age1 - bacon_error1,
                      ymax = bacon_age1 + bacon_error1), alpha = 0.7) +
    geom_errorbarh(aes(y = bacon_age1,
                       xmax = 1950 - age + e.young,
                       xmin = 1950 - age - e.older,
                       x = 1950 - age), alpha = 0.7) +
    geom_point(aes(x = 1950 - age, y = bacon_age1), alpha = 0.9) +
    coord_equal(xlim = c(1800, 2000), ylim = c(1800, 2000), expand = c(0,0)) +
    scale_x_reverse() +
    xlab('Reported 210Pb Age') +
    ylab('Bacon Estimated Age') +
    theme_bw() +
    theme(axis.title.x = element_text(family = 'serif', 
                                      face = 'bold.italic', 
                                      size = 18),
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
  
  
  return(list(plot = lead_plot, data = bacon_bound))
  
}