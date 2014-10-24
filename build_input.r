# Write Cores files for use in Bacon runs
# Modify files to include paleon pre-settlement sample
# Build bacon_params file containing Bacon run parameters

require(neotoma)
require(raster)
require(fields)
require(sp)

source('write_agefile_stepps.r')
source('utils/helpers.r')

###########################################################################################
# read in data
###########################################################################################

pollen_meta <- read.csv('../data/pollen/pollen_meta_2014-07-22.csv', header=TRUE)
pre_depths  <- read.csv(file='data/cal_data_mid_depth_2014-10-23.csv', stringsAsFactors=FALSE)

clh_meta   <- read.csv('data/hotchkiss_lynch_calcote_meta.csv', header=TRUE)
clh_counts <- read.csv('data/hotchkiss_lynch_calcote_counts.csv', header=TRUE, stringsAsFactors=FALSE)

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

###########################################################################################
# neotoma cores
###########################################################################################

pollen_meta <- get_survey_year(pollen_meta)

ids    = pollen_meta$datasetID
ncores = length(pollen_dat)

bacon.params <- data.frame(handle     = sapply(pollen_dat, function(x)x$metadata$dataset$collection.handle),
                           dataset.id = sapply(pollen_dat, function(x)x$metadata$dataset$dataset.id),
                           acc.mean.mod  = 3.02,
                           acc.mean.old  = 15,
                           acc.shape.mod = 0.53,
                           acc.shape.old = 0.9,
                           mem.strength  = 4,
                           mem.mean = 0.7,
                           hiatus   = NA,
                           thick    = 5,
                           age.type = sapply(pollen_dat, function(x) unique(x$sample.meta$AgeType)),
                           run      = FALSE,
                           suit     = sapply(pollen_dat, function(x) ifelse(unique(x$sample.meta$AgeType) == 'Varve years BP', FALSE, NA)), 
                           ndates   = NA,
                           success  = NA,
                           stringsAsFactors=FALSE)



for(i in 1:ncores){ 
  print(i)
  site.handle <- as.vector(bacon.params$handle[i])
  site.id     <- bacon.params$dataset.id[i]
  age.out      = try(write_agefile_stepps(download    = download, 
                                          survey.year = survey.year, 
                                          chronology  = chronology, 
                                          path        = path, 
                                          corename    = corename, 
                                          site.id     = site.id, 
                                          cal.prog    = cal.prog))
  if (!class(age.out)=='try-error'){
    bacon.params$suit[i]   = age.out$run_flag
    bacon.params$ndates[i] = age.out$ndates
  } else {
    bacon.params$suit[i] = FALSE
  }
}

# check to see why some fail
bacon.params[bacon.params$suit==FALSE,]

write.table(bacon.params, file='bacon.params.csv', col.names=TRUE, row.names=FALSE, sep=',')

###########################################################################################
# calcote cores
###########################################################################################

long_cores <- c('Ferr01X', 'Hell Hole', 'Lone02', 'Warner')
handles    <- c('FERRY', 'HELLHOLE', 'LONE', 'WARNER')
ids        <- c('CLH2', 'CLH3', 'CLH5', 'CLH6')

clh_meta   <- clh_meta[clh_meta$name %in% long_cores, ]
clh_counts <- clh_counts[clh_counts$name %in% long_cores, ]
clh_meta   <- get_survey_year(clh_meta)

ncores = length(long_cores)

bacon.params <- data.frame(handle        = handles,
                           dataset.id    = ids,
                           acc.mean.mod  = 3.02,
                           acc.mean.old  = 15,
                           acc.shape.mod = 0.53,
                           acc.shape.old = 0.9,
                           mem.strength  = 4,
                           mem.mean = 0.7,
                           hiatus   = NA,
                           thick    = 5,
                           age.type = 'Radio',
                           run      = FALSE,
                           suit     = TRUE, 
                           ndates   = NA,
                           success  = NA,
                           stringsAsFactors=FALSE)

# add in the pre-samples
for (i in 1:ncores){
  
  handle = bacon.params$handle[i]
  print(handle)
  
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