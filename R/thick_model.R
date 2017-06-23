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
    select(name, handle, thick, pol_age_max, age_type, core_length1)
  
  uncalib <- thick$age_type %in% "Radiocarbon years BP" & 
    thick$pol_age_max < 46000 &
    thick$pol_age_max > 75
  
  recalib <- BchronCalibrate(thick$pol_age_max[uncalib],
                             rep(100, sum(uncalib)),
                             calCurves = rep('intcal13', sum(uncalib)))
  
  re_ages <- recalib %>% map(function(x)sum(x$ageGrid * x$densities)) %>% unlist
  
  thick$pol_age_max[uncalib] <- re_ages
  
  thick_glm <- anova(glm(thick ~ core_length1, data = thick), test = "F")
                    
  thick_model <- ggplot(aes(x = core_length1, y = thick), data = thick) + 
    geom_jitter(alpha = 0.4, height = 0.5) +
    geom_smooth(method = 'glm', method.args = list(family = 'Gamma'), se = TRUE) +
    theme_bw() +
    xlab("Maximum Core Depth (cm)") +
    ylab("Best-Fit Model Section Thickness (cm)") +
    coord_cartesian(ylim = c(4, 21), expand = FALSE) +
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
  
  return(list(plot = thick_model, model = model, glm = thick_glm))
  
}