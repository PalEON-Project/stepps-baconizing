#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(raster)
require(fields)
require(sp)

source('R/config.r')
source('Bacon.R')
source('R/utils/helpers.r')

setup = FALSE

if (setup){
  # remove files from any previous runs
  unlink('Cores/*/*.txt')
  unlink('Cores/*/*.out')
  unlink('Cores/*/*.bacon')
  unlink('Cores/*/*.pdf')
  source('setup_bacon.r')
}

bacon_params <- read.csv(paste0('data/bacon_params_v', version, '.csv'), header=TRUE, sep=',')
pollen_meta  <- read.csv(paste0('data/pollen_meta_v', version, '.csv'), header=TRUE, sep=',')

# # find depth span to determine if there is a minimum number of thicknesses that work
# ncores = nrow(bacon_params)
# lengths = rep(NA, ncores)
# for (i in 1:ncores){
#   site.params = bacon_params[i,]
#   # find hiatus depth
#   if (site.params$suit == TRUE){
#     geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)
#     lengths[i] = max(geochron$depth) - min(geochron$depth)
#   }  
# }
# sum(is.na(lengths))
# lengths[!is.na(lengths) & (lengths<20)]
# bacon_params$handle[!is.na(lengths) & (lengths<20)]
# bacon_params$handle[is.na(lengths)] # should be 17 problem sites

# 
# #############################################################################################
# # find depth span to determine if there is a minimum number of thicknesses that work
# ncores = nrow(bacon_params)
# setts = rep(FALSE, ncores)
# for (i in 1:ncores){
#   site.params = bacon_params[i,]
#   # find hiatus depth
#   if (site.params$suit == TRUE){
#     geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)
#     setts[i] = any(substr(geochron$labid,1,4) == 'Sett')
#   }  
# }
# 
# i_check = which(setts == TRUE)
#############################################################################################
run_batch <- function(bacon_params, suff){

  for(i in 1:nrow(bacon_params)){
    #for(i in ids_rerun){
    
    print(i)
    site.params <- bacon_params[i,]
    if (!(site.params$bacon)){next}
    thick = site.params$thick
    suff = thick
    
    site.params$mem.strength = 2
    site.params$mem.mean     = 0.5
    # site.params$thick        = thick
    
    # HANSEN and KERR LAKE both have crazy high accumulation rate
    if (site.params$dataset.id %in% c(1004, 15916)){
      site.params$acc.mean.old = 100
    }
    
    site.params <- run.bacon(site.params)
    
  }
}

# only rerun for certain thicks
pollen_meta_v6 <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_thick_v6.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')
thicks = pollen_meta_v6$thick[match(bacon_params$dataset.id, pollen_meta_v6$id)]
bacon_params$thick = pollen_meta_v6$thick[match(bacon_params$dataset.id, pollen_meta_v6$id)]
pollen_meta$thick  = pollen_meta_v6$thick[match(bacon_params$dataset.id, pollen_meta_v6$id)]
bacon_params$bacon = pollen_meta$bacon[match(bacon_params$dataset.id, pollen_meta$dataset_id)]

run_batch(bacon_params, suff=NULL)

# overwrite pollen_meta file?
write.table(pollen_meta, file=paste0('data/pollen_meta_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')
