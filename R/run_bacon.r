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
site_data    <- read.csv(paste0('data/pollen_site_age_meta_v', version, '.csv'), header=TRUE, sep=',')

# find depth span to determine if there is a minimum number of thicknesses that work
ncores = nrow(bacon_params)
lengths = rep(NA, ncores)
for (i in 1:ncores){
  site.params = bacon_params[i,]
  # find hiatus depth
  if (site.params$suit == TRUE){
    geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)
    lengths[i] = max(geochron$depth) - min(geochron$depth)
  }  
}
sum(is.na(lengths))
lengths[!is.na(lengths) & (lengths<20)]
bacon_params$handle[!is.na(lengths) & (lengths<20)]
bacon_params$handle[is.na(lengths)] # should be 17 problem sites


#############################################################################################
# find depth span to determine if there is a minimum number of thicknesses that work
ncores = nrow(bacon_params)
setts = rep(FALSE, ncores)
for (i in 1:ncores){
  site.params = bacon_params[i,]
  # find hiatus depth
  if (site.params$suit == TRUE){
    geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)
    setts[i] = any(substr(geochron$labid,1,4) == 'Sett')
  }  
}

i_check = which(setts == TRUE)
#############################################################################################
run_batch <- function(ncores, thicks, bacon_params, suff){
  
  # write.table(t(colnames(bacon_params)), file=paste0('bacon.fit.hiatus_', suff,'.csv'), sep=',', append=FALSE, col.names=FALSE, row.names=FALSE)
  
  if (length(thicks == 1)) {thicks = rep(thicks, ncores)}
  
  for(i in 1:ncores){
    #for(i in ids_rerun){
    
    thick = thicks[i]
    
    print(i)
    site.params <- bacon_params[i,]
    
    site.params$mem.strength = 2
    site.params$mem.mean     = 0.5
    site.params$thick        = thick
    
    # HANSEN has a crazy high accumulation rate
    if (site.params$dataset.id == 1004){
      site.params$acc.mean.old = 100
    }
    
    # KERR LAKE has a crazy high accumulation rate
    if (site.params$dataset.id == 15916){
      site.params$acc.mean.old = 100
    }
    
    site.params <- run.bacon(site.params)
    
    # write.table(site.params, paste0('bacon.fit.hiatus_', suff,'.csv'), sep=',', append=TRUE, col.names=FALSE, row.names=FALSE)
    
  }
}

# only rerun for certain thicks
pollen_meta_v6 <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_thick_v6.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')
thicks = pollen_meta_v6$thick[match(bacon_params$dataset.id, pollen_meta_v6$id)]

for (i in 1:ncores) {
  thick = thicks[i]
  run_batch(ncores, thick, bacon_params, suff=as.character(thick))
}

# # # run_batch(ncores, thicks$thick, bacon_params, suff='opt')
# # thicks = c(5, 10, 15, 20)
# thicks=c(5)
# for (thick in thicks) {
#   run_batch(ncores, thick, bacon_params, suff=as.character(thick))
# }

# after we choose the thicknesses, make the pdf of plots
pollen_meta_v6 <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_thick_v6.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')
# pollen_meta <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_all_thicks_umw_v', version, '.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')

pollen_meta <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_v6.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')

pollen_meta$bacon = site_data$bacon[match(pollen_meta$id, site_data$dataset_id)]
pollen_meta = pollen_meta[!is.na(pollen_meta$bacon),]
pollen_meta = pollen_meta[(pollen_meta$bacon == TRUE),]

pollen_meta_v6 = pollen_meta_v6[substr(pollen_meta_v6$id, 1, 3) != 'CLH', ]

pollen_meta$thick = pollen_meta_v6$thick[match(pollen_meta$id, as.numeric(pollen_meta_v6$id))]
write.table(pollen_meta, file=paste0('data/pollen_meta_thick_v', version ,'.csv'), col.names=TRUE, row.names=FALSE, sep=',')
