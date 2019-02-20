# Returns a plot comparing sequential ages and BChron calibrated dates for 
# artificial cores.
#
# Does not return anything, simply saves an SVG file

compare_recalibrations <- function() {

  oldest <- seq(1000, 21000, by = 1000)
  depths <- 0:100
  
  model.recal <- function(max.age, depths){
    # This is plotting out the changes between actual and interpolated dates when ages 
    # are directly recalibrated.
    ages <- seq(71, max.age, length.out = length(depths))
    calibed <- BchronCalibrate(ages = round(ages, 0), 
                               ageSds = rep(1, length(ages)), 
                               calCurves = rep('intcal13', length(ages)))
    
    from.calib <- sapply(calibed, function(x)weighted.mean(x$ageGrid, x$densities))
    lin.model <- approx(x = depths[c(1, length(depths))], 
                        y = from.calib[c(1, length(depths))],
                        xout = depths)
    data.frame(depths  = depths,
               direct  = from.calib,
               linear  = lin.model$y,
               max.age = factor(max.age))
  }
  
  out.tests <- ldply(oldest, model.recal, depths = depths)
  
  out.tests$diff <- out.tests$direct - out.tests$linear
  
  diff  <- ggplot(out.tests, aes(x = linear, y = diff, color = max.age)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 21000), expand = FALSE) +
    geom_hline(aes(yintercept = 0)) +
    xlab('Linear Model with Recalibration') +
    ylab('Model Difference') +
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
  
  ggsave(filename = 'figures/agediff_plot.svg', diff)
  
  return(NULL)
}