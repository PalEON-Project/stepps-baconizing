lead_plots <- function(geochron_tables) {

  widen <- function(x) {
    data.frame(x$dataset$site.data,
               x$geochron,
               row.names = NULL)
  }
  
  wide_table <- geochron_tables %>% 
    purrr::map(widen) %>% bind_rows
  
  leads <- wide_table %>% filter(geo.chron.type %in% "Lead-210")
  
  # There are some ages with improperly named age types.
  leads$age[!is.na(leads$age) & leads$age > 500] <- 1950 - 
    leads$age[!is.na(leads$age) & leads$age > 500]
  
  # None of the ages reported as Calendar years BP are wrong.
  
  lead_smooth <- ggplot(leads %>% filter(e.older > 0), aes(x = age, y = e.older)) +
    geom_point(alpha = 0.3) + 
    geom_smooth(aes(x = age, y = e.older)) +
    coord_cartesian(xlim = c(-60, 200), ylim = c(0, 150), expand = c(0, 0)) +
    scale_y_sqrt() +
    xlab("Years Before Present") +
    ylab("Error - SD") +
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
  
  
  lead_na <- ggplot(leads[is.na(leads$e.older),], aes(x = age)) + 
    geom_histogram(fill = 'red', alpha = 0.5, color = 'black') +
    xlab("Years Before Present") +
    ylab("Unassigned Samples") +
    
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
  
  
  return(grid.arrange(lead_smooth, lead_na, ncol = 1))
  
}