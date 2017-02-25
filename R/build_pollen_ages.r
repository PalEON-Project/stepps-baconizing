library(neotoma)
library(ggplot2)
library(maps)
library(dplyr)
library(purrr)

source('R/utils/compile_lists.r')
source('R/utils/build_pollen_ts_helpers.r')
source('R/config.r')

# pol_version    <- '7'
add_varves     <- TRUE
model          <- 'Bacon'

ndraws         <- 500 # number of posterior samples 

bacon_out_path <- '../stepps-baconizing'

###########################################################################################################
# user inputs
###########################################################################################################

states      <- c('wisconsin', 
                 'minnesota', 
                 'michigan:north', 
                 'michigan:south')

pollen_meta <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_v', version, '.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')

# read in dictionaries
pollen.equiv.stepps <- read.csv("pollen.equiv.stepps.csv", stringsAsFactors = F)
pollen.equiv        <- read.csv("pollen.equiv.csv", stringsAsFactors = F, 
                                sep = ',', row.names = NULL)

ids    <- pollen_meta$dataset_id
state  <- pollen_meta$state
nsites <- length(ids)

# load list containing pollen counts
# will load object called pollen2k
# the first time takes a while to pull from Neotoma
pol = NA

if (file.exists(paste0('data/pollen_stepps_', version, '.rdata'))) {
  # loads object pollen2k
  load(paste0('data/pollen_stepps_', version, '.rdata'))
} 
if (! paste0('pol_stepps_', version, '.rds') %in% list.files('data') | (length(ids)!=length(pol))){
  
  # download and save the raw data
  pol <- neotoma::get_download(ids)
  saveRDS(pol, file=paste0('data/pol_stepps_', version, '.rds'))
  
  # miss = list()
  # aggregate the taxa
  pollen_stepps = list()
  pollen_stepps <- pol %>% 
    map(function(x) compile_list_neotoma(x, 'Stepps', pollen.equiv)) %>% 
    map(function(x) compile_list_stepps(x, 
                                        list.name='must_have', 
                                        pollen.equiv.stepps, 
                                        cf = TRUE, 
                                        type = TRUE))
    
    # dff <- map2_df(pollen_stepps, state, ~ mutate(.x, state = .y))
    for (i in 1:length(pollen_stepps)){
      pollen_stepps[[i]]$dataset$site.data$state = state[i]
    }
  save(pollen_stepps, file=paste0('data/pollen_stepps_', version, '.rdata'))
  print("Update version in config file!")
} 

taxa = sort(unique(pollen.equiv.stepps$must_have))
taxa = taxa[!is.na(taxa)]

###########################################################################################################
# Read _samples.csv files and compute posterior means
###########################################################################################################

# pol = pol_stepps

bchron = TRUE
nsites = length(pollen_stepps)
pollen_ts  = list()

for (i in 1:nsites){  
  print(i)
  
  x = pollen_stepps[[i]]
  
  id       = x$dataset$dataset.meta$dataset.id
  
  if (id %in% vids){
    print("Skipping varve core.")
    next
  }
  
  lat      = x$dataset$site.data$lat
  long     = x$dataset$site.data$long
  altitude = x$dataset$site.data$elev
  state    = x$dataset$site.data$state
  handle   = x$dataset$dataset.meta$collection.handle
  sitename = gsub("[ ']", "", as.vector(x$dataset$site.data$site.name))
  sitename = gsub("[ ,]", "", sitename)
  
  if (id == 1004) {
    print('Hansen is messy. Discard!')
    next
  }
  
  age_default = x$sample.meta$age 
  
  thick = pollen_meta$thick[pollen_meta$dataset_id == id]
  thick_handle = pollen_meta$handle[pollen_meta$dataset_id == id]
  
  if (!is.na(thick)){
    fname = sprintf('%s/Cores/%s/%s_%s_samples.csv', bacon_out_path, thick_handle, thick_handle, thick)
    
    if (handle != thick_handle) {
      print(paste0('Why are the handles changing for ', handle))
    }
    
    if (file.exists(fname)){
      age_bacon = read.table(fname, sep=',', header=TRUE)
    } else {
      print(paste0('Site suitable, but no Bacon samples found for ', handle))
      next
    }
    
  } else {
    print(paste0('Site ', handle, ' unsuitable.'))
    next
  }
  
  depth = age_bacon$depths
  age_bacon = age_bacon[,-1]
  
  draws = sample(seq(1,ncol(age_bacon)), size=ndraws)
  age_post_bacon = age_bacon[,draws]
  colnames(age_post_bacon) = sapply(seq(1,ndraws), function(x) paste0("bacon_draw", x))
  
  age_bacon = apply(age_bacon, 1, mean)
  
  counts  = x$counts
  counts  = counts[x$sample.meta$depth %in% depth,]
  age_default = age_default[x$sample.meta$depth %in% depth]
  
  if (bchron) {
    fname = sprintf('%s/Cores/%s/%s_bchron_samples.csv', bacon_out_path, thick_handle, thick_handle)
    if (file.exists(fname)){
      age_bchron = read.table(fname, sep=',', header=TRUE)
      
      age_bchron = age_bchron[age_bchron$depths %in% depth, ]
      depth_bchron = age_bchron$depths
      age_bchron = age_bchron[,-1]
      
      draws = sample(seq(1,ncol(age_bchron)), size=ndraws)
      
      age_post_bchron = age_bchron[,draws]
      colnames(age_post_bchron) = sapply(seq(1,ndraws), function(x) paste0("bchron_draw", x))
      
      age_bchron  = apply(age_bchron, 1, mean)
    } else {
      age_bchron = NA
      age_post_bchron = data.frame(matrix(NA, nrow=1, ncol=ndraws))
      colnames(age_post_bchron) = sapply(seq(1,ndraws), function(x) paste0("bchron_draw", x))
      
    }
  }
  
  meta = data.frame(id       = id, 
                    sitename = sitename, 
                    #handle   = handle, 
                    lat      = lat, 
                    long     = long, 
                    state    = state, 
                    altitude = altitude)
  
  if (all(is.na(meta))){
    print(paste0("problem with site", i))
  }
  
  if (sum(x$sample.meta$depth %in% depth) == 1) {
    ncounts = 1
  } else if (sum(x$sample.meta$depth %in% depth) > 1) {
    ncounts = nrow(counts)
  }
  
  meta = meta[rep(1, ncounts),] 
  
  if (length(age_bacon) == ncounts) {
    if (ncounts == 1){
      counts = t(counts)
    }
    
    if (bchron){
      pollen_ts = rbind(pollen_ts, cbind(meta, age_bacon, age_bchron, age_default, age_post_bacon, age_post_bchron, depth, counts)) 
    } else {
      pollen_ts = rbind(pollen_ts, cbind(meta, age_bacon, age_post_bacon, age_default, depth, counts)) 
    }
  } else {
    print("Number of ages differs from the number of samples. Skipping core.")
    next
  }
}

###########################################################################################################
# add varves if desired
###########################################################################################################
if (add_varves){
  
  prec = read.csv('data/neotoma_age-model-precedence.csv', header=TRUE)
  
  # vids read in from config file
  # vids = c(3131, 2309, 14839, 546, 1643, 2309, 1649)
  vids = varves$id
  state_v = varves$state
  
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
    
    age_post_bacon = replicate(ndraws, age_bacon)
    colnames(age_post_bacon) = sapply(seq(1,ndraws), function(x) paste0("bacon_draw", x))
    
    age_bchron = age_default # not really bacon ages ...
    
    age_post_bchron = replicate(ndraws, age_bchron)
    colnames(age_post_bchron) = sapply(seq(1,ndraws), function(x) paste0("bchron_draw", x))
    
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
    pollen_ts = rbind(pollen_ts, cbind(meta, age_bacon, age_bchron, age_default, age_post_bacon, age_post_bchron, depth, counts)) 
  }
}

pollen_ts = pollen_ts[!is.na(pollen_ts$id),]

###########################################################################################################
# write the data; still thinking about the best way to do this!
###########################################################################################################

# write.table(pollen_ts, file=paste('data/pollen_ts_bacon_', Sys.Date(), '.csv', sep=''), quote=FALSE, row.names=FALSE)
if (add_varves){
  suff='_varves'
} else {
  suff=''
}
write.table(pollen_ts, file=paste0('data/sediment_ages_v', version, suff, '.csv'), quote=FALSE, row.names=FALSE, sep=',')

# try splitting out the draws into RDS
bacon_draws = pollen_ts[,grep('bacon_draw', colnames(pollen_ts))]

for (i in 1:ncol(bacon_draws)) {
  dir.create(paste0('data/bacon_ages_v', version), showWarnings = FALSE)
  saveRDS(bacon_draws[,i], file=paste0('data/bacon_ages_v', version,'/draw', i, '.rds'))
}

# try splitting out the draws into RDS
bchron_draws = pollen_ts[,grep('bchron_draw', colnames(pollen_ts))]

for (i in 1:ncol(bchron_draws)) {
  dir.create(paste0('data/bchron_ages_v', version), showWarnings = FALSE)
  saveRDS(bchron_draws[,i], file=paste0('data/bchron_ages_v', version, '/draw', i, '.rds'))
}

#
# 
# pollen_ts_meta = pollen_ts[,-(grep('draw', colnames(pollen_ts)))]
# write.table(pollen_ts_meta, file=paste0('data/bacon_ages/pollen_ts_bacon_meta_', version, '.csv'), quote=FALSE, row.names=FALSE, sep=',')
# 
# # # check with older version
# # pollen_meta_old = read.table(file=paste0('data/bacon_ages/pollen_ts_bacon_meta_v2.csv'), sep=',', header=TRUE)
