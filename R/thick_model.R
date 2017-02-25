allan_thick <- function() {
  thickness <- read.csv('data/pollen_meta_v1.0.csv')

  thick_model <- ggplot(aes(x = thick, y = pol_age_max), data = thickness) + 
    geom_point() + 
    geom_smooth() +
    theme_bw()
  
  model <- gam(pol_age_max ~ s(thick, k = 3), data = thickness)
  
  return(list(plot = thick_model, model = model))
  
}