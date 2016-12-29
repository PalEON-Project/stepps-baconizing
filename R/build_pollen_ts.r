library(neotoma)
library(ggplot2)
library(maps)

source('utils/compile_list_stepps.r')
# source('utils/build_pollen_ts_helpers.r')

###########################################################################################################
# user inputs
###########################################################################################################

states = c('wisconsin', 'minnesota', 'michigan:north')

pollen_meta <- read.csv('data/pollen_meta_2014-07-22.csv', header=TRUE, stringsAsFactors=FALSE, sep=',')
# read in dictionaries
pollen.equiv.stepps = read.csv("data/pollen.equiv.stepps.csv", stringsAsFactors=F)
pollen.equiv.new    = read.csv("data/pollen.equiv.csv", stringsAsFactors=F, sep=',', row.names=NULL)

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

###########################################################################################################
# use neotoma package to pull UMW data
###########################################################################################################

taxa = sort(unique(pollen.equiv.stepps$must_have))
taxa = taxa[!is.na(taxa)]

pollen_meta = split_mi(pollen_meta, longlat=TRUE)
ids         = pollen_meta$datasetID
# pollen_meta = pollen_meta[pollen_meta$state2 %in% states,]

pollen2k = list()
for (i in 1:length(pollen_dat)){  
  idx = which(ids == pollen_dat[[i]]$dataset$dataset.meta$dataset.id)
  
  pollen2k[[i]] = compile_taxa_stepps(pollen_dat[[i]], list.name='WhitmoreSmall', alt.table=NULL, cf=TRUE, type=TRUE)
  pollen2k[[i]] = whitmore2stepps(pollen2k[[i]], taxa)
  pollen2k[[i]]$dataset$site.data$state = pollen_meta$state2[idx]
}

save(pollen2k, file='data/pollen2k.rdata')

###########################################################################################################
# Read _samples.csv files and compute posterior means
###########################################################################################################

n = length(pollen2k)

pollen_ts  = list()

for (i in 1:n){  
  print(i)
  
  x = pollen2k[[i]]
  
  id       = x$dataset$dataset.meta$dataset.id
  lat      = x$dataset$site.data$lat
  long     = x$dataset$site.data$long
  altitude = x$dataset$site.data$elev
  state    = x$dataset$site.data$state
  sitename = x$dataset$site.data$site.name
  handle   = x$dataset$dataset.meta$collection.handle
  type     = x$sample.meta$age.type
  
  age_old = x$sample.meta$age 
  
  fname = sprintf('Cores/%s/%s_samples.csv', handle, handle)
  if (file.exists(fname)){
    age = read.table(sprintf('Cores/%s/%s_samples.csv', handle, handle), sep=',', 
                     header=TRUE)
  } else {
    print(paste0('No Bacon samples for ', handle))
    next
  }
  
  bacon_depth = age$depth
  age = age[,-1]
  age = apply(age, 1, mean)
  
  counts  = x$counts
  counts  = counts[x$sample.meta$depth %in% bacon_depth,]
  age_old = age_old[x$sample.meta$depth %in% bacon_depth]
  
  meta = data.frame(id       = id, 
                    sitename = sitename, 
                    handle   = handle, 
                    lat      = lat, 
                    long     = long, 
                    state    = state, 
                    altitude = altitude, 
                    type     = type)
  
  meta = meta[rep(1, nrow(counts)),] 
  
  if (length(age) == nrow(counts)) {
    pollen_ts = rbind(pollen_ts, cbind(meta, age, age_old, bacon_depth, counts)) 
  } else {
    print("Number of ages differs from the number of samples. Skipping core.")
    next
  }
}

###########################################################################################################
# Add CLH sites
###########################################################################################################

long_cores = c('Ferr01X', 'Hell Hole', 'Lone02', 'Warner')
handles    = c('FERRY', 'HELLHOLE', 'LONE', 'WARNER')
ids        = c('CLH2', 'CLH3', 'CLH5', 'CLH6')

n = length(long_cores)

for (i in 1:n){
  
  site_meta   = clh_meta[clh_meta$name==long_cores[i], ]
  site_counts = clh_counts[clh_counts$name==long_cores[i], ]
  
  id       = ids[i]
  lat      = site_meta$lat
  long     = site_meta$long
  altitude = site_meta$Elev.m.
  state    = site_meta$state
  sitename = site_meta$name
  handle   = handles[i]
  type     = 'Radiocarbon'
  
  age_old = site_counts$age

  
  fname = sprintf('Cores/%s/%s_samples.csv', handle, handle)
  if (file.exists(fname)){
    age = read.table(sprintf('Cores/%s/%s_samples.csv', handle, handle), sep=',', 
                     header=TRUE)
  } else {
    print(paste0('No Bacon samples for ', handle))
    next
  }
  
  bacon_depth = age$depths
  age = age[,-1]
  age = apply(age, 1, mean)
  
  counts = site_counts[,8:ncol(site_counts)]
  x = compile_taxa_stepps(counts, list.name='Stepps', alt.table=pollen.equiv.new, cf=TRUE, type=TRUE)
  x = compile_taxa_stepps(x, list.name='must_have', alt.table=pollen.equiv.stepps, cf = TRUE, type = TRUE)
  x = x[,!(colnames(x)=='Other')]
  
  # add empty columns for missing taxa
  zero_taxa = taxa[!(taxa %in% colnames(x))]
  add_back   = matrix(0, nrow=nrow(x), ncol=length(zero_taxa))
  colnames(add_back) = zero_taxa
  
  tmp    = cbind(x, add_back)
  counts = tmp[, sort(colnames(tmp))]

  counts  = counts[site_counts$depth_mid %in% bacon_depth,]
  age_old = age_old[site_counts$depth_mid %in% bacon_depth]

  meta = data.frame(id       = id, 
                    sitename = sitename, 
                    handle   = handle, 
                    lat      = lat, 
                    long     = long, 
                    state    = state, 
                    altitude = altitude, 
                    type     = type)
  meta = meta[rep(1, nrow(counts)),] 
  
  if (length(age)==nrow(counts)){
    pollen_ts = rbind(pollen_ts, cbind(meta, age, age_old, bacon_depth, counts)) 
  } else {
    print("Number of ages differs from the number of samples. Skipping core.")
    next
  }
}

###########################################################################################################
# write the data
###########################################################################################################

write.table(pollen_ts, file=paste('data/pollen_ts_bacon_', Sys.Date(), '.csv', sep=''), quote=FALSE, row.names=FALSE)
