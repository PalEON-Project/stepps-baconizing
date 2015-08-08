#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(raster)
require(fields)
require(sp)

source('Bacon.R')
source('utils/helpers.r')

setup = FALSE

if (setup){
  # remove files from any previous runs
  unlink('Cores/*/*.txt')
  unlink('Cores/*/*.out')
  unlink('Cores/*/*.bacon')
  unlink('Cores/*/*.pdf')
  source('setup_bacon.r')
}

bacon.params <- read.csv('bacon.params.csv', header=TRUE, sep=',')

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


# run bacon!
thicks = c(5, 10, 15, 20)
thicks = c(15, 20)
for (thick in thicks){
  
  write.table(t(colnames(bacon.params)), file=paste0('bacon.fit.hiatus_', thick,'.csv'), sep=',', append=FALSE, col.names=FALSE, row.names=FALSE)
  
  for(i in 1:ncores){
    
    print(i)
    site.params <- bacon.params[i,]
    
    site.params$mem.strength = 2
    site.params$mem.mean     = 0.5
    site.params$thick        = thick
    
    # i=8; 102
    #   if (site.params$dataset.id %in% c(241, 1665)){
    #     site.params$thick        = 10
    #   }
    
    #   # some sites don't work with thick=5
    #   # can we programmatically deal with this?
    #   if (site.params$dataset.id %in% c(774, 3485)){
    #     site.params$thick = 3
    #   } else if (site.params$dataset.id %in% c(313)){
    #     site.params$thick = 2
    #   }
    
    site.params <- run.bacon(site.params)
    
    write.table(site.params, paste0('bacon.fit.hiatus_', thick,'.csv'), sep=',', append=TRUE, col.names=FALSE, row.names=FALSE)
  }
  
  # compile all individual pdfs into one pdf
  fnames = bacon.params$handle[which(bacon.params$suit == 1)]
  fname_str = sapply(fnames, function(x) paste0('Cores/', x, '/*_', thick,'.pdf'))
  fname_str = paste(fname_str, collapse = ' ')
  
  sys_str = paste0("gs -sDEVICE=pdfwrite -o bacon_fit_plots_hiatus_v2_", thick, ".pdf ", fname_str)
  system(sys_str)
  
}


## write.table(bacon.params, file='bacon.params.csv', col.names=TRUE)
# system("gs -sDEVICE=pdfwrite -o bacon_fit_plots_hiatus.pdf Cores/*/*.pdf")
