compare_recalibrations <- function() {

  oldest <- seq(1000, 21000, by = 1000)
  depths <- 0:100
  
  model.recal <- function(max.age, depths){
    # This is plotting out the changes between actual and interpolated dates when ages 
    # are directly recalibrated.  It's mostly just to 
    ages <- seq(71, max.age, length.out = length(depths))
    calibed <- BchronCalibrate(ages, 
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
  
  direct <- ggplot(out.tests, aes(x = direct, y = linear, color = max.age)) +
    geom_line() +
    scale_x_continuous(limits = c(0, 21000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 21000), 
                       expand = c(0, 0)) +
    xlab('Direct Recalibration') +
    ylab('Linear Model with\n Recalibration') +
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
  
  diff  <- ggplot(out.tests, aes(x = linear, y = diff, color = max.age)) +
    geom_line() +
    scale_x_continuous(limits = c(0, 21000), expand = c(0, 0)) +
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
  
  return(grid.arrange(direct, diff))
}