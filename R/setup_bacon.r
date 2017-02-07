#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(neotoma)
require(maps)
require(fields)
require(raster)
require(rgdal)

# sp is required by raster, loaded by default
# require(sp)

source('R/config.r')
source('Bacon.R')
source('R/utils/helpers.r')
source('R/utils/write_agefile_stepps.r')

# pollen_meta <- read.csv('data/pollen_meta_2014-07-22.csv', header=TRUE)
# pollen_meta <- get_survey_year(pollen_meta)
# # for some reason the datasetID changed for Jones Lake
# pollen_meta[pollen_meta$datasetID == 1394, 'datasetID'] = 15274
# # for some reason the datasetID changed for Canyon Lake
# pollen_meta[pollen_meta$datasetID == 3055, 'datasetID'] = 15682
# ids_old = pollen_meta$datasetID

gpids <- get_table(table.name='GeoPoliticalUnits')
gpid = c(gpids[which(gpids$GeoPoliticalName == 'Minnesota'),1],
         gpids[which(gpids$GeoPoliticalName == 'Wisconsin'),1],
         gpids[which(gpids$GeoPoliticalName == 'Michigan'),1])

dl = list()
for (i in 1:length(gpid)){
  dl = c(dl, get_dataset(datasettype='pollen', gpid=gpid[i], ageyoung=0))
}

ids = as.numeric(names(dl))
nsites  = length(ids)

# load list containing pollen counts
# will load object called pollen2k
# the first time takes a while to pull from Neotoma
pol = NA
if (file.exists(paste0('data/pol_', my_date, '.rdata'))) {
  # loads object pollen2k
  load(paste0('data/pol_', my_date, '.rdata')) 
} 
if (!file.exists(paste0('data/pol_', my_date, '.rdata'))|(length(ids)!=length(pol))){
  
  # download and save the raw data
  pol = list()
  for (i in 1:nsites){ 
    print(i)
    
    # if (id == 1394) id = 15274
    
    pol[[i]] = get_download(ids[i])[[1]]
  }  
  save(pol, file=paste0('data/pol_', my_date, '.rdata'))
} 

# extract the meta data
pollen_meta = data.frame(id     = sapply(pol, function(x) x$dataset$dataset.meta$dataset.id), 
                         handle  = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
                         lat     = sapply(pol, function(x) x$dataset[[1]]$lat),
                         long    = sapply(pol, function(x) x$dataset[[1]]$long))
pollen_meta$state = map.where(database="state", x=pollen_meta$long, y=pollen_meta$lat)

pollen_meta = split_mi(pollen_meta, longlat=TRUE)
pollen_meta$state = pollen_meta$state2
pollen_meta       = get_survey_year(pollen_meta)

write.table(pollen_meta, file=paste0('data/pollen_meta_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')


# create meta data object
site_age_data <- data.frame(handle      = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
                            name        = sapply(pol, function(x)x$dataset$site.data$site.name),
                            dataset_id  = sapply(pol, function(x)x$dataset$dataset.meta$dataset.id),
                            pol_age_min = sapply(pol, function(x) min(x$sample.meta$age)),
                            pol_age_max = sapply(pol, function(x) max(x$sample.meta$age)),
                            age_type    = sapply(pol, function(x) x$sample.meta$age.type[1]),
                            bacon      = rep(NA, nsites),
                            gc_age_min = rep(NA, nsites),
                            gc_age_max = rep(NA, nsites),
                            reason     = rep(NA, nsites),
                            new        = rep(NA, nsites),
                            amb_rise   = rep(NA))

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
    bacon_params$suit[i] = TRUE
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
                                     site.id     = site.id))
  
  if (!class(bacon_out)=='try-error'){
    site_age_data$bacon[i]      = bacon_out$run_flag
    site_age_data$gc_age_max[i] = bacon_out$gc_age_max
    site_age_data$gc_age_min[i] = bacon_out$gc_age_min
    site_age_data$amb_rise[i]   = bacon_out$amb_rise
    
    bacon_params$suit[i]   = bacon_out$run_flag
    bacon_params$ndates[i] = bacon_out$ndates
  } else {
    bacon_params$suit[i] = FALSE
  }
  
  # these are varves that we want to skip
  if (site.id %in% c(3131, 2309, 14839, 546, 1643, 2309)){
    bacon_params$suit[i] = FALSE
  }
  # curl breaks without this
  # Sys.sleep(3)
}

# check to see why some fail
bacon_params[bacon_params$suit==FALSE,c(1,2,11,14)]
bacon_params[bacon_params$suit==TRUE,c(1,2,11)]
# 
# why are some NA
site_age_data[which(is.na(site_age_data$bacon)),]
# 
# which(site_age_data$bacon == FALSE)
# site_age_data[which(site_age_data$bacon == FALSE),]
site_age_data[which(site_age_data$bacon == TRUE),]
# 
# length(which(site_age_data$bacon == TRUE))
# site_age_data$dataset.id[which(site_age_data$bacon == TRUE)] %in% ids_old
# sum(site_age_data$dataset.id[which(site_age_data$bacon == TRUE)] %in% ids_old)
# 
# site_age_data$new = !(site_age_data$dataset_id %in% ids_old)
# site_age_data     = site_age_data[with(site_age_data, order(-bacon, new)),]

write.table(site_age_data, file=paste0('data/pollen_site_age_meta_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')
write.table(bacon_params, file=paste0('data/bacon_params_v', version, '.csv'), col.names=TRUE, row.names=FALSE, sep=',')
