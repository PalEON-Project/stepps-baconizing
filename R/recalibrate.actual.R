#  Get all the chronologies and rebuild as before:


if(!'chronologies.RData' %in% list.files('data/output/')){
  chronologies <- list()
  for(i in 1:length(all.downloads)){
    chronologies <- try(get_chroncontrol(all.downloads))
  }
  save(chronologies, file = 'data/chronologies.RData')
} else{
  load('data/chronologies.RData')
}

rebuild <- function(x){
  # This function works to rebuild the age models for cores, it tests first to see if there
  # is a built age model (that's the call to `meta` in the first if statement).
  
  if ('meta' %in% names(chronologies[[x]])) {
    
    # If there's a model and it's radiocarbon years, then we directly calibrate it t
    # calibrated radiocarbon years using Bchron.  
    if (chronologies[[x]]$meta$age.type == 'Radiocarbon years BP' &
         nrow(chronologies[[x]]$chron.control) > 1) {
      
      ages <- all.downloads[[x]]$sample.meta$age
      good.ages <- ages > 71 & ages < 40000 & !is.na(ages)
      
      if (sum(good.ages) > 0) {
        calibed <- BchronCalibrate(ages[good.ages],
                                   ageSds    = rep(1, sum(good.ages)),
                                   calCurves = rep('intcal13', sum(good.ages)))

        from.calib <- sapply(calibed, function(x)weighted.mean(x$ageGrid, x$densities))
        
        controls <- chronologies[[x]]$chron.control$age
        errors   <- abs(chronologies[[x]]$chron.control$age - chronologies[[x]]$chron.control$age.young)
    
        calib <- !chronologies[[x]]$chron.control$control.type %in% 
          c('Core top', 'Annual laminations (varves)', 'Lead-210', 'Radiocarbon, calibrated') &
           controls < 40000 & controls > 71

        errors[is.na(errors)] <- 200
        
        if (sum(calib) > 0) {
          lin.ages <- BchronCalibrate(controls[calib],
                                      ageSds = errors[calib],
                                      calCurves = rep('intcal13', sum(calib)))
          
          controls[calib] <- sapply(lin.ages, function(x)weighted.mean(x$ageGrid, x$densities))
          
        }

        new.ages <- approx(x    = chronologies[[x]]$chron.control$depth, 
                           y    = controls,
                           xout = all.downloads[[x]]$sample.meta$depth)
        
        output <- data.frame(core       = all.downloads[[x]]$dataset$site$site.name,
                             handle     = all.downloads[[x]]$dataset$dataset.meta$collection.handle,
                             age.direct = from.calib,
                             age.lin    = new.ages$y[good.ages])
      } else {
        # If there's no data then we push out an NA (but still keep the site name, for tracking)
        
        output <- data.frame(core       = all.downloads[[x]]$dataset$site.data$site.name,
                             handle     = all.downloads[[x]]$dataset$dataset.meta$collection.handle,
                             age.direct = NA,
                             age.lin    = NA)
      }
    } else {
      output <- data.frame(core = all.downloads[[x]]$dataset$site$site.name,
                           handle = all.downloads[[x]]$dataset$dataset.meta$collection.handle,
                           age.direct = NA,
                           age.lin = NA)
    }
    
  } else {
    output <- data.frame(core = all.downloads[[x]]$dataset$site$site.name,
                         handle = all.downloads[[x]]$dataset$dataset.meta$collection.handle,
                         age.direct = NA,
                         age.lin = NA)
  }
  output
}

#  This is just to debug, so I can see where errors occur.
#  for(i in 1:length(chronologies))rebuild(i)

if (!'new.chrons.RData' %in% list.files('data/output')) {
  # Build the chronologies.  This then saves to a file so we don't have to re-run it every time.
  
  build.chrons <- lapply(1:length(chronologies), function(x){
                           test <- try(rebuild(x))
                           if (class(test) == 'try-error') {
                                 
                            test <- data.frame(core       = NA, 
                                               handle     = NA,
                                               age.direct = NA,
                                               age.lin    = NA)
                           }
                           
                           return(test)})
  
  new.chrons      <- do.call(rbind.data.frame, new.chrons)
  new.chrons$diff <- new.chrons$age.lin - new.chrons$age.direct
  
  
  save(new.chrons, file = 'data/output/new.chrons.RData')

} else {
  load('data/output/new.chrons.RData')
}

