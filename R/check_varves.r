prec = read.csv('data/neotoma_age-model-precedence.csv', header=TRUE)


source('R/config.r')

# vids read in from config file
# vids = c(3131, 2309, 14839, 546, 1643, 2309, 1649)
vids = varves$id
state_v = varves$state

pollen_ts  = list()

for (i in 1:length(vids)){
  x = get_download(vids[i])[[1]]
  
  convert1 = compile_list_neotoma(x, 'Stepps', pollen.equiv)
  x = compile_list_stepps(convert1, list.name='must_have', pollen.equiv.stepps, cf = TRUE, type = TRUE)
  
  id       = x$dataset$dataset.meta$dataset.id
  lat      = x$dataset$site.data$lat
  long     = x$dataset$site.data$long
  altitude = x$dataset$site.data$elev
  state    = state_v[i]
  handle   = x$dataset$dataset.meta$collection.handle
  sitename = gsub("[ ']", "", as.vector(x$dataset$site.data$site.name))
  
  counts  = x$counts
  
  # age_types = sapply(x$chronologies, function(x) x$age.type[1])
  # best_default = which.min(prec$precedence[match(age_types, prec$age_type)])
  # 
  # if (best_default > 1){
  #   print(paste0("Adjusted default! Previous default was in ", age_types[1]))
  # }
  # 
  # # chron.controls <- get_chroncontrol(x$chronologies[best_default][[1]]$chronology.id[1], 
  # #                                    verbose = FALSE)
  
  age_default = x$chronologies[[varves$chron[i]]]$age 
  age_bacon = age_default # not really bacon ages ...
  age_bchron = age_default # not really bacon ages ...
  
  meta = data.frame(id       = id, 
                    sitename = sitename, 
                    #handle   = handle, 
                    lat      = lat, 
                    long     = long, 
                    state    = state, 
                    altitude = altitude)
  
  meta = meta[rep(1, nrow(counts)),] 
  depth = rep(NA, nrow(counts))
  # pollen_ts = rbind(pollen_ts, cbind(meta, age_bacon, age_post_bacon, age_default, depth, counts)) 
  pollen_ts = rbind(pollen_ts, cbind(meta, age_bacon, age_bchron, age_default, depth, counts)) 
}

library(analogue)

dat_site = pollen_ts[which(pollen_ts$id == 2309),c('age_default', 'depth', taxa)]

Stratiplot(age_default ~ . - depth, data = dat_site, type = c("h","l","g"), title='Ruby')

dat_site = pollen_ts[which(pollen_ts$id == 546),c('age_default', 'depth', taxa)]
Stratiplot(age_default ~ . - depth, data = dat_site, type = c("h","l","g"), title)

dat_site = pollen_ts[which(pollen_ts$id == 1643),c('age_default', 'depth', taxa)]
Stratiplot(age_default ~ . - depth, data = dat_site, type = c("h","l","g"))
