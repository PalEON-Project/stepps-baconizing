#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(raster)
require(fields)
require(sp)

source('Bacon.R')
source('utils/helpers.r')

setup = FALSE
version = 5

if (setup){
  # remove files from any previous runs
  unlink('Cores/*/*.txt')
  unlink('Cores/*/*.out')
  unlink('Cores/*/*.bacon')
  unlink('Cores/*/*.pdf')
  source('setup_bacon.r')
}

bacon.params <- read.csv(paste0('data/bacon_params_umw_v', version, '.csv'), header=TRUE, sep=',')
site_data <- read.csv(paste0('data/pollen_site_meta_umw_v', version, '.csv'), header=TRUE, sep=',')

# find depth span to determine if there is a minimum number of thicknesses that work
ncores = nrow(bacon.params)
lengths = rep(NA, ncores)
for (i in 1:ncores){
  site.params = bacon.params[i,]
  # find hiatus depth
  if (site.params$suit == TRUE){
    geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)
    lengths[i] = max(geochron$depth) - min(geochron$depth)
  }  
}
sum(is.na(lengths))
lengths[!is.na(lengths) & (lengths<20)]
bacon.params$handle[!is.na(lengths) & (lengths<20)]
bacon.params$handle[is.na(lengths)] # should be 17 problem sites


#############################################################################################
# find depth span to determine if there is a minimum number of thicknesses that work
ncores = nrow(bacon.params)
setts = rep(FALSE, ncores)
for (i in 1:ncores){
  site.params = bacon.params[i,]
  # find hiatus depth
  if (site.params$suit == TRUE){
    geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)
    setts[i] = any(substr(geochron$labid,1,4) == 'Sett')
  }  
}

i_check = which(setts == TRUE)
#############################################################################################

# run bacon!
all_thicks = TRUE

if (all_thicks){
  thicks = c(5, 10, 15, 20)
  # thicks = c(15, 20)
} else {
  thicks = read.csv('bacon.fit.thick.csv')$thick
}


# i_check = which(bacon.params$dataset.id %in% site_data$dataset_id[which(site_data$amb_rise == TRUE)])
# i_check = which(bacon.params$dataset.id %in% c(14626, 14933, 15032, 15269, 15660))


# write.table(t(colnames(bacon.params)), file=paste0('bacon.fit.hiatus_', thick,'.csv'), sep=',', append=FALSE, col.names=FALSE, row.names=FALSE)

run_batch <- function(ncores, thicks, bacon.params, suff){
  
  # write.table(t(colnames(bacon.params)), file=paste0('bacon.fit.hiatus_', suff,'.csv'), sep=',', append=FALSE, col.names=FALSE, row.names=FALSE)
  
  if (length(thicks == 1)) {thicks = rep(thick, ncores)}
  
  for(i in 1:ncores){
    #for(i in ids_rerun){
    
    thick = thicks[i]
    
    print(i)
    site.params <- bacon.params[i,]
    
    site.params$mem.strength = 2
    site.params$mem.mean     = 0.5
    site.params$thick        = thick
    
    # this site has a crazy high accumulation rate
    if (site.params$dataset.id == 1004){
      site.params$acc.mean.old = 100
    }
    
    site.params <- run.bacon(site.params)
    
    # write.table(site.params, paste0('bacon.fit.hiatus_', suff,'.csv'), sep=',', append=TRUE, col.names=FALSE, row.names=FALSE)
    
  }
}

# run_batch(ncores, thicks$thick, bacon.params, suff='opt')
thicks = c(5, 10, 15, 20)
thicks=c(20)
for (thick in thicks) {
  run_batch(ncores, thick, bacon.params, suff=as.character(thick))
}

# after we choose the thicknesses, make the pdf of plots
pollen_meta <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_all_thicks_umw_v', version, '.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')
pollen_meta = pollen_meta[pollen_meta$bacon == TRUE,]


# fix this!!!
# compile all individual pdfs into one pdf
thick = 'opt'
fnames = pollen_meta$handle
thick = pollen_meta$thick
# meta = data.frame(fname=fnames, thick=thicks[which(bacon.params$suit == 1),'thick'])
# fname_str = sapply(cbind(fnames, thick), function(x) paste0('Cores/', x, '/', x, '_', ,'.pdf'))

fname_str = apply(cbind(fnames, thick), 1, function(x) paste0('Cores/', x[1], '/', x[1], '_', x[2],'.pdf'))
fname_str = paste(fname_str, collapse = ' ')

sys_str = paste0("gs -sDEVICE=pdfwrite -o bacon_fit_plots_hiatus_v", version, ".pdf ", fname_str)
system(sys_str)


## write.table(bacon.params, file='bacon.params.csv', col.names=TRUE)
# system("gs -sDEVICE=pdfwrite -o bacon_fit_plots_hiatus.pdf Cores/*/*.pdf")
