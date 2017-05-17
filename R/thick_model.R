allan_thick <- function() {
  thickness <- readr::read_csv('data/pollen_meta_v1.0.csv')

  thick <- thickness %>% purrrlyr::by_row(function(x){
    dir  <- paste0('Cores/',x$handle, '/')
    file <- paste0(x$handle, '_depths.txt')
    if(file %in% list.files(dir)) {
      depths <- readr::read_delim(paste0(dir, file), 
                                  delim = ',',
                                  col_names = FALSE)
      output <- diff(range(depths))
    } else {
      output <- NA
    }
    return(data.frame(core_length= as.numeric(output)))}, .collate = 'cols') %>% 
    select(name, handle, thick, core_length1)
  
  thick_model <- ggplot(aes(x = thick, y = core_length1), data = thick) + 
    geom_jitter(alpha = 0.4, width=.2) +
    geom_smooth(method = 'gam', method.args = list(family = 'poisson', k = 2), se = TRUE) +
    theme_bw() +
    xlab("Best-Fit Model Section Thickness (cm)") +
    ylab("Core Length (cm)") +
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
  
  model <- gam(pol_age_max ~ s(thick, k = 3), family = 'poisson', data = thickness)
  
  return(list(plot = thick_model, model = model))
  
}