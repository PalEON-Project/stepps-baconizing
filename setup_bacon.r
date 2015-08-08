#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(neotoma)
require(raster)
require(fields)
require(sp)

source('Bacon.R')
source('utils/helpers.r')
source('utils/write_agefile_stepps.r')

pollen_meta <- read.csv('data/pollen_meta_2014-07-22.csv', header=TRUE)
pollen_meta <- get_survey_year(pollen_meta)

# for some reason the datasetID changed for Jones Lake
pollen_meta[pollen_meta$datasetID == 1394, 'datasetID'] = 15274

ids = pollen_meta$datasetID

# load list containing pollen counts
# will load object called pollen_dat
# the first time takes a while to pull from Neotoma
if (!file.exists('data/pollen_list.rdata')){
  pollen_dat <- get_download(ids)
  save(pollen_dat, file='data/pollen_list.rdata')
} else {
  # loads object pollen_dat
  load('data/pollen_list.rdata') 
}
# load_pollen_data(pollen_meta$datasetID)
ncores = length(pollen_dat)

bacon.params <- data.frame(handle = sapply(pollen_dat, function(x)x$dataset$dataset.meta$collection.handle),
                           dataset.id = sapply(pollen_dat, function(x)x$dataset$dataset.meta$dataset.id),
                           acc.mean.mod = 3.02,
                           acc.mean.old = 15,
                           acc.shape.mod = 0.53,
                           acc.shape.old = 0.9,
                           mem.strength = 4,
                           mem.mean = 0.7,
                           hiatus = NA,
                           #                            thick = sapply(pollen_dat, function(x) min(diff(x$sample.meta$depths)[diff(x$sample.meta$depths)>0])),
                           thick = 5,
                           age.type = sapply(pollen_dat, function(x) unique(x$sample.meta$age.type)),
                           run = FALSE,
                           suit = sapply(pollen_dat, function(x) ifelse(unique(x$sample.meta$age.type) == 'Varve years BP', FALSE, NA)), 
                           ndates = NA,
                           success = NA,
                           stringsAsFactors=FALSE)

# 101 gives error still
for(i in 1:ncores){ 
  print(i)
  site.handle <- as.vector(bacon.params$handle[i])
  print(site.handle)
  site.id <- bacon.params$dataset.id[i]
  
  #for debugging
  download=pollen_dat[[i]]
  survey.year=pollen_meta$set.year[i]
  chronology=1
  path='.'
  site.id=site.id
  corename=site.handle
  cal.prog='Bacon'
#   
#   x=get_download(site.id)
#   x[[1]]$sample.meta
#   x[[1]]$chronologies
  
  age.out = try(write_agefile_stepps(download=download, survey.year=survey.year, chronology=chronology, path=path, corename=corename, 
                                     site.id=site.id, cal.prog=cal.prog))
  if (!class(age.out)=='try-error'){
    bacon.params$suit[i] = age.out$run_flag
    bacon.params$ndates[i] = age.out$ndates
    bacon.params$dates[i] = age.out$dates
  } else {
    bacon.params$suit[i] = FALSE
  }
}

# check to see why some fail
bacon.params[bacon.params$suit==FALSE,]

write.table(bacon.params, file='bacon.params.csv', col.names=TRUE, row.names=FALSE, sep=',')

long_cores = c('Ferr01X', 'Hell Hole', 'Lone02', 'Warner', 'Lily')
handles    = c('FERRY', 'HELLHOLE', 'LONE', 'WARNER', 'LILY')
ids        = c('CLH2', 'CLH3', 'CLH5', 'CLH6', 'CLH4')

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
                           #                            thick = sapply(pollen_dat, function(x) min(diff(x$sample.meta$depths)[diff(x$sample.meta$depths)>0])),
                           thick = 5,
                           age.type = 'Radio',
                           run = FALSE,
                           suit = TRUE, 
                           ndates = NA,
                           success = NA,
                           stringsAsFactors=FALSE)

# add in the pre-samples
pre_depths = read.csv(file='data/cal_data_mid_depth_2014-07-28.csv', stringsAsFactors=FALSE)
for (i in 1:ncores){
  handle = bacon.params$handle[i]
  id = bacon.params$dataset.id[i]
  if (any(pre_depths$id == bacon.params$dataset.id[i])){
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
  } 
  
  depths = clh_counts[which(clh_counts$name==long_cores[i]), 'depth_mid']
  write.table(depths, paste0(path, "/Cores/", handle, "/", 
                             handle, "_depths.txt"), col.names = FALSE, row.names = FALSE, quote=TRUE)
  
}

write.table(bacon.params, file='bacon.params.csv', sep=',', append=TRUE, col.names=FALSE, row.names=FALSE)