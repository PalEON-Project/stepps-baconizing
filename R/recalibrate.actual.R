#  Get all the chronologies and rebuild as before:


if(!'chronologies.RData' %in% list.files('data/output/')){
  chronologies <- list()
  for(i in 1:length(all.downloads)){
    chronologies[[i]] <- try(get_chroncontrol(all.downloads[[i]]$sample.meta$chronology.id[1]))
  }
  save(chronologies, file='data/chronologies.RData')
} else{
  load('data/chronologies.RData')
}

rebuild <- function(x){
  if('meta' %in% names(chronologies[[x]])){
    if(chronologies[[x]]$meta$age.type == 'Radiocarbon years BP' &
         nrow(chronologies[[x]]$chron.control) > 1){
      
      ages <- all.downloads[[x]]$sample.meta$age
      good.ages <- ages > 0 & ages < 40000 & !is.na(ages)
      
      if(sum(good.ages) > 0){
        calibed <- BchronCalibrate(ages[good.ages],
                                   ageSds=rep(1, sum(good.ages)),
                                   calCurves=rep('intcal13', sum(good.ages)))
        
        from.calib <- sapply(calibed, function(x)weighted.mean(x$ageGrid, x$densities))
        
        controls <- chronologies[[x]]$chron.control$age
        errors   <- abs(chronologies[[x]]$chron.control$age - chronologies[[x]]$chron.control$age.young)
    
        calib <- !chronologies[[x]]$chron.control$control.type %in% 
          c('Core top', 'Annual laminations (varves)', 'Lead-210', 'Radiocarbon, calibrated') &
          controls < 40000 & controls > 0
        
        errors[is.na(errors)] <- 200
        
        if(sum(calib) > 0){
          lin.ages <- BchronCalibrate(controls[calib],
                                      ageSds=errors[calib],
                                      calCurves=rep('intcal13', sum(calib)))
          controls[calib] <- sapply(lin.ages, function(x)weighted.mean(x$ageGrid, x$densities))
          
        }
        
        new.ages <- approx(x=chronologies[[x]]$chron.control$depth, y= controls,
                           xout = all.downloads[[x]]$sample.meta$depth)
        
        output <- data.frame(core = all.downloads[[x]]$dataset$site$site.name,
                             handle = all.downloads[[x]]$dataset$dataset.meta$collection.handle,
                             age.direct = from.calib,
                             age.lin = new.ages$y[good.ages])
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
if('new.chrons.RData' %in% list.files('data/')){
  new.chrons <- ldply(1:length(chronologies), rebuild)
  new.chrons$diff <- new.chrons$age.lin - new.chrons$age.direct
  save(new.chrons, file='new.chrons.RData')
} else {
  load('data/new.chrons.RData')
}

