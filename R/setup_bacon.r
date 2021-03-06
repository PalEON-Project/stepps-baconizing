#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
library(neotoma)
library(maps)
library(fields)
library(raster)
library(rgdal)
library(dplyr)

# sp is required by raster, loaded by default
# require(sp)

source('R/config.r')
source('Bacon.R')
source('R/utils/helpers.r')
source('R/utils/write_agefile_stepps.r')

# old note about dataset id changes
# dataset id change for Jones Lake: old=1394, new=15274
# dataset id change for Canyon Lake: old=3055, new=15682

dl <- get_dataset(datasettype='pollen', gpid=c('Minnesota', 'Wisconsin', 'Michigan'), ageyoung=0)

ids = as.numeric(names(dl))
nsites  = length(ids)

# load list containing pollen counts
# will load object called pollen2k
# the first time takes a while to pull from Neotoma

if (file.exists(paste0('data/pollen_v', version, '.rds'))) {
  pol <- readRDS(paste0('data/pollen_v', version, '.rds'))
} 
if (!file.exists(paste0('data/pollen_v', version, '.rdata'))|(length(ids)!=length(pollen))){
  pol <- get_download(dl)

  saveRDS(pol, file=paste0('data/pollen_v', version, '.rds'))
}

# extract the meta data
pollen_meta = data.frame(dataset_id  = sapply(pol, function(x) x$dataset$dataset.meta$dataset.id), 
                         name        = sapply(pol, function(x)x$dataset$site.data$site.name),
                         handle      = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
                         lat         = sapply(pol, function(x) x$dataset[[1]]$lat),
                         long        = sapply(pol, function(x) x$dataset[[1]]$long),
                         state       = NA,
                         age_type    = sapply(pol, function(x) x$sample.meta$age.type[1]),
                         bacon       = NA,
                         amb_rise    = NA,
                         pol_age_min = sapply(pol, function(x) min(x$sample.meta$age)),
                         pol_age_max = sapply(pol, function(x) max(x$sample.meta$age)),
                         gc_age_min = NA,
                         gc_age_max = NA,
                         reason     = NA,
                         new        = NA)

# get the state, and then split mi:up and mi:lp
pollen_meta$state = map.where(database="state", x=pollen_meta$long, y=pollen_meta$lat)
pollen_meta       = split_mi(pollen_meta, longlat=TRUE)
pollen_meta$state = pollen_meta$state2
# get the mininum survey year, used to assign date to presettlement samples
pollen_meta$set.year = get_survey_year(pollen_meta, pollen_meta$state)

# do we need this still?
# generate object that contains bacon inputs for each core
bacon_params <- data.frame(handle = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
                           dataset.id = sapply(pol, function(x)x$dataset$dataset.meta$dataset.id),
                           acc.mean.mod = 3.02,
                           acc.mean.old = 15,
                           acc.shape.mod = 0.53,
                           acc.shape.old = 0.9,
                           mem.strength = 4,
                           mem.mean = 0.7,
                           hiatus = NA,
                           #                            thick = sapply(pol, function(x) min(diff(x$sample.meta$depths)[diff(x$sample.meta$depths)>0])),
                           thick = 5,
                           age.type = sapply(pol, function(x) unique(x$sample.meta$age.type)),
                           run = FALSE,
                           suit = sapply(pol, function(x) ifelse(unique(x$sample.meta$age.type) == 'Varve years BP', FALSE, NA)), 
                           ndates = NA,
                           success = NA,
                           stringsAsFactors=FALSE)

# write the chron control files for bacon
for(i in 1:nsites){ 
  print(i)
  site.handle <- as.vector(bacon_params$handle[i])
  print(site.handle)
  site.id <- as.numeric(as.vector(bacon_params$dataset.id[i]))
  
  # these are varves that we want to skip
  if (site.id == 1649){
    print("Site has manual reservoir corrections. Do not alter Bacon chron control file!")
    bacon_params$suit[i] = FALSE
    pollen_meta$bacon[i] = FALSE
    next
  }
  
  download    = pol[[i]]
  survey.year = pollen_meta$set.year[i]
  path        = '.'
  corename    = site.handle
  site.id     = site.id
  
  bacon_out = try(write_agefile_stepps(download    = pol[[i]], 
                                     survey.year = pollen_meta$set.year[i], 
                                     path        = '.', 
                                     corename    = site.handle, 
                                     site.id     = site.id,
                                     stepps      = TRUE))
  
  if (!class(bacon_out)=='try-error'){
    pollen_meta$bacon[i]      = bacon_out$run_flag
    pollen_meta$gc_age_max[i] = bacon_out$gc_age_max
    pollen_meta$gc_age_min[i] = bacon_out$gc_age_min
    pollen_meta$amb_rise[i]   = bacon_out$amb_rise
    
    bacon_params$suit[i]   = bacon_out$run_flag
    bacon_params$ndates[i] = bacon_out$ndates
  } else {
    bacon_params$suit[i] = FALSE
  }
  
  # these are varves that we want to skip
  if (site.id %in% vids){
    bacon_params$suit[i] = FALSE
    pollen_meta$bacon[i] = FALSE
  }
}

# check to see why some fail
bacon_params[bacon_params$suit==FALSE,c(1,2,11,14)]
bacon_params[bacon_params$suit==TRUE,c(1,2,11)]
# 
# why are some NA
pollen_meta[which(is.na(pollen_meta$bacon)),]
# 
# which(pollen_meta$bacon == FALSE)
# pollen_meta[which(pollen_meta$bacon == FALSE),]
pollen_meta[which(pollen_meta$bacon == TRUE),]

write.table(pollen_meta, file=paste0('data/pollen_meta_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')
write.table(bacon_params, file=paste0('data/bacon_params_v', version, '.csv'), col.names=TRUE, row.names=FALSE, sep=',')