allan_thick <- function() {
  thickness <- readr::read_csv('data/pollen_meta_v1.0.csv')

  thick_model <- ggplot(aes(x = thick, y = pol_age_max), data = thickness) + 
    geom_point() + 
    geom_smooth() +
    theme_bw() +
    xlab("Best-Fit Model Section Thickness") +
    ylab("Maximum Core Age") +
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
  
  model <- gam(pol_age_max ~ s(thick, k = 3), data = thickness)
  
  return(list(plot = thick_model, model = model))
  
}