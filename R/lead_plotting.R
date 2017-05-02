lead_plots <- function(geochron_tables) {

  binford <- data.frame(age = -60 + c(10, 100, 150),
                        nf_mn  = c(1, 10, 80)/2,
                        nf_mx  = c(2, 20, 90)/2)
  
  widen <- function(x) {
    data.frame(x$dataset$site.data,
               x$geochron,
               row.names = NULL)
  }
  
  wide_table <- geochron_tables %>% 
    purrr::map(widen) %>% bind_rows
  
  leads <- wide_table %>% filter(geo.chron.type %in% "Lead-210")
  
  # There are some ages with improperly named age types.
  wrong_age <- !is.na(leads$age) & leads$age > 500
  
  leads$age[wrong_age] <- 1950 - leads$age[wrong_age]
  
  # None of the ages reported as Calendar years BP are wrong.
  
  filter_data <- leads %>% filter(e.older > 0)
  
  lead_smooth <- ggplot() +
    geom_rect(data = binford, aes(xmin = age - 5, xmax = age + 5, 
                                  ymin = nf_mn, ymax = nf_mx), fill = 'red') +
    geom_point(data = filter_data, aes(x = age, y = e.older), alpha = 0.3) + 
    geom_smooth(data = filter_data, aes(x = age, y = e.older), method = 'glm', method.args = list(family = 'poisson')) +
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