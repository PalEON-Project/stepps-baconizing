#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(neotoma)
require(raster)
require(fields)
require(sp)
require(maps)

source('Bacon.R')
source('utils/helpers.r')
source('utils/write_agefile_stepps2.r')

version=3

pollen_meta <- read.csv('data/pollen_meta_2014-07-22.csv', header=TRUE)
pollen_meta <- get_survey_year(pollen_meta)
# for some reason the datasetID changed for Jones Lake
pollen_meta[pollen_meta$datasetID == 1394, 'datasetID'] = 15274
# for some reason the datasetID changed for Canyon Lake
pollen_meta[pollen_meta$datasetID == 3055, 'datasetID'] = 15682
ids_old = pollen_meta$datasetID


gpids <- get_table(table.name='GeoPoliticalUnits')
gpid = c(gpids[which(gpids$GeoPoliticalName == 'Minnesota'),1],
         gpids[which(gpids$GeoPoliticalName == 'Wisconsin'),1],
         gpids[which(gpids$GeoPoliticalName == 'Michigan'),1])
dl = get_dataset(datasettype='pollen', gpid=c(gpid), ageyoung=0)

all_ids = as.numeric(names(dl))
nsites = length(all_ids)

# load list containing pollen counts
# will load object called pollen2k
# the first time takes a while to pull from Neotoma
pol=NA
if (file.exists(paste0('data/pollen_list_v', version, '.rdata'))) {
  # loads object pollen2k
  load(paste0('data/pollen_list_v', version, '.rdata')) 
} 
if (!file.exists(paste0('data/pollen_list_v', version, '.rdata'))|(length(all_ids)!=length(pol))){
  
  # download and save the raw data
  pol = list()
  for (i in 1:nsites){ 
    id = all_ids[i]
    print(id)
    # if (id == 1394) id = 15274
    pol[[i]] = get_download(id)[[1]]
  }  
  save(pol, file=paste0('data/pollen_list_v', version, '.rdata'))
} 

pollen_meta_all= data.frame(id = sapply(pol, function(x) x$dataset$site.data$site.id), 
                            handle = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
                            lat = sapply(pol, function(x) x$dataset[[1]]$lat),
                            long = sapply(pol, function(x) x$dataset[[1]]$long))
pollen_meta_all$state = map.where(database="state", x=pollen_meta_all$long, y=pollen_meta_all$lat)

pollen_meta_all = split_mi(pollen_meta_all, longlat=TRUE)
pollen_meta_all$state = pollen_meta_all$state2
pollen_meta_all <- get_survey_year(pollen_meta_all)



ncores = length(pol)

site_data <- data.frame(handle = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
                        name = sapply(pol, function(x)x$dataset$site.data$site.name),
                        dataset_id = sapply(pol, function(x)x$dataset$dataset.meta$dataset.id),
                        pol_age_min = sapply(pol, function(x) min(x$sample.meta$age)),
                        pol_age_max = sapply(pol, function(x) max(x$sample.meta$age)),
                        age_type = sapply(pol, function(x) max(x$sample.meta$age.type)),
                        bacon = rep(NA, ncores),
                        gc_age_min = rep(NA, ncores),
                        gc_age_max = rep(NA, ncores),
                        reason = rep(NA, ncores),
                        new = rep(NA, ncores),
                        amb_rise = rep(NA))


bacon.params <- data.frame(handle = sapply(pol, function(x)x$dataset$dataset.meta$collection.handle),
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

# check = c(13060, 13069, 13071, 13073, 14446)
# i_check = which(bacon.params$dataset.id %in% check)
which(bacon.params$dataset.id %in% c(14626, 14933, 15032, 15269, 15660))

# 101 gives error still
for(i in 1:ncores){ 
  print(i)
  site.handle <- as.vector(bacon.params$handle[i])
  print(site.handle)
  site.id <- as.numeric(as.vector(bacon.params$dataset.id[i]))
  
  #for debugging
  download=pol[[i]]
  survey.year=pollen_meta_all$set.year[i]
  chronology=1
  path='.'
  site.id=site.id
  corename=site.handle
  cal.prog='Bacon'
  
  x=get_download(site.id)
  x[[1]]$sample.meta
  x[[1]]$chronologies
  
  age.out = try(write_agefile_stepps2(download=download, survey.year=survey.year, chronology=chronology, path=path, corename=corename, 
                                     site.id=site.id, cal.prog=cal.prog))
  if (!class(age.out)=='try-error'){
    site_data$bacon[i]      = age.out$run_flag
    site_data$gc_age_max[i] = age.out$gc_age_max
    site_data$gc_age_min[i] = age.out$gc_age_min
    site_data$amb_rise[i] = age.out$amb_rise
    
    bacon.params$suit[i]   = age.out$run_flag
    bacon.params$ndates[i] = age.out$ndates
  } else {
    bacon.params$suit[i] = FALSE
  }
}

# check to see why some fail
bacon.params[bacon.params$suit==FALSE,]

# why are some NA
site_data[which(is.na(site_data$bacon)),]

which(site_data$bacon == FALSE)
site_data[which(site_data$bacon == FALSE),]
site_data[which(site_data$bacon == TRUE),]

length(which(site_data$bacon == TRUE))
site_data$dataset.id[which(site_data$bacon == TRUE)] %in% ids_old
sum(site_data$dataset.id[which(site_data$bacon == TRUE)] %in% ids_old)

site_data$new = !(site_data$dataset_id %in% ids_old)
site_data = site_data[with(site_data, order(-bacon, new)),]

write.table(site_data, file=paste0('data/pollen_site_meta_umw_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')



write.table(bacon.params, file=paste0('data/bacon_params_umw_v', version, '.csv'), col.names=TRUE, row.names=FALSE, sep=',')

#####################################################################################################################################

long_cores = c('Lone02', 'Lily04')
handles    = c('LONE', 'LILY04')
ids        = c('CLH5', 'CLH4')

clh_meta   = read.csv('data/hotchkiss_lynch_calcote_meta_v0.1.csv', header=TRUE)
clh_counts = read.csv('data/hotchkiss_lynch_calcote_counts_v0.1.csv', header=TRUE, stringsAsFactors=FALSE)

clh_meta   = clh_meta[clh_meta$name %in% long_cores, ]
clh_counts = clh_counts[clh_counts$name %in% long_cores, ]
clh_meta   = get_survey_year(clh_meta)

ncores = length(long_cores)

bacon.params <- data.frame(handle = handles,
                           dataset.id = ids,
                           acc.mean.mod = 3.02,
                           acc.mean.old = 15,
                           acc.shape.mod = 0.53,
                           acc.shape.old = 0.9,
                           mem.strength = 4,
                           mem.mean = 0.7,
                           hiatus = NA,
                           #                            thick = sapply(pol, function(x) min(diff(x$sample.meta$depths)[diff(x$sample.meta$depths)>0])),
                           thick = 5,
                           age.type = 'Radio',
                           run = FALSE,
                           suit = TRUE, 
                           ndates = NA,
                           success = NA,
                           stringsAsFactors=FALSE)

path='.'

# add in the pre-samples
pre_depths = read.csv(file='data/cal_data_mid_depth_2014-07-28.csv', stringsAsFactors=FALSE)
for (i in 1:ncores){
  handle = bacon.params$handle[i]
  id = bacon.params$dataset.id[i]
  if (any(pre_depths$id == bacon.params$dataset.id[i])){
    amb_rise = TRUE
    chron <- read.table(sprintf('Cores/%s/%s.csv', handle, handle), sep=',', header=TRUE)
    if (!any(substr(chron$labid, 1, 4) == 'Pres')){
      labid = 'Presettlement_paleon'
      age   = 1950 - 1907 # want to make sure we get the presettlement date!
      error = 50
      depth = pre_depths$depth[which(pre_depths$id == bacon.params$dataset.id[i])]
      cc    = 0   
      chron <- rbind(chron, data.frame(labid, age, error, depth, cc))
      chron <- chron[with(chron, order(age)),]
      
      write.csv(chron, sprintf('Cores/%s/%s.csv', handle, handle), row.names = FALSE, quote=TRUE)
    }
    gc_age_max = max(chron$age)
    gc_age_min = min(chron$age)
  } 
  
  ages = clh_counts[which(clh_counts$name==long_cores[i]), 'age']
  pol_age_min = min(ages)
  pol_age_max = max(ages)
  
  depths = clh_counts[which(clh_counts$name==long_cores[i]), 'depth_mid']
  write.table(depths, paste0(path, "/Cores/", handle, "/", 
                             handle, "_depths.txt"), col.names = FALSE, row.names = FALSE, quote=TRUE)
  
  site_data = rbind(site_data,
                    data.frame(handle = handles[i],
                               name  = handles[i],
                               dataset_id = ids[i],
                               pol_age_min = pol_age_min,
                               pol_age_max = pol_age_max,
                               age_type    = 'CLH',
                               bacon       = TRUE,
                               gc_age_min  = gc_age_min,
                               gc_age_max  = gc_age_max,
                               reason      = NA,
                               new         = FALSE,
                               amb_rise = amb_rise))
  
  pollen_meta_all = rbind(pollen_meta_all, 
                          data.frame(id = ids[i],
                                     handle = handles[i],
                                     lat = clh_meta$lat[i],
                                     long = clh_meta$long[i],
                                     state = 'wisconsin',
                                     state2 = 'wisconsin',
                                     x = clh_meta$x[i],
                                     y = clh_meta$y[i],
                                     set.year = 1907))
  
}

write.table(pollen_meta_all, file=paste0('data/pollen_meta_all_umw_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')

write.table(site_data, file=paste0('data/pollen_site_meta_umw_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')

write.table(bacon.params, file=paste0('data/bacon_params_umw_v', version, '.csv'), sep=',', append=TRUE, col.names=FALSE, row.names=FALSE)


# # plot core intervals
# site_data$core_num = seq(1, nrow(site_data))
# library(ggplot2)
# ggplot(data=site_data, aes(x=pol_age_min, y=core_num)) + geom_segment(aes(xend=pol_age_max, yend=core_num, colour=bacon, linetype=new)) 
# 
# ggsave('figures/core_intervals.pdf')