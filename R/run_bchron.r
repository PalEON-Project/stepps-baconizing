#  Note, put this code in a directory with a Bacon.R file and a Cores directory.
require(raster)
require(fields)
require(sp)
require('Bchron')

source('R/utils/helpers.r')

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

bacon_params <- read.csv(paste0('data/bacon_params_umw_v', version, '.csv'), header=TRUE, sep=',')
site_data    <- read.csv(paste0('data/pollen_site_meta_umw_v', version, '.csv'), header=TRUE, sep=',')

ncores = nrow(bacon_params)

# 82 breaks things- doesn't complete
# 165 breaks things - doesn't complete
# 173 breaks things - error
# 241 breaks things - error

# run_bchron <- function(site_params){
for (i in 242:ncores){
  print(paste0(i, '; ', bacon_params[i,]$handle))
  
  site_params = bacon_params[i,]
  
  # check for suitability
  if (site_params$suit==1){
    
    # find hiatus depth
    geochron = read.table(sprintf('Cores/%s/%s.csv', site_params$handle, site_params$handle), sep=',', header=TRUE)  
    depths   = scan(sprintf('Cores/%s/%s_depths.txt', site_params$handle, site_params$handle))
    
    calCurves = rep(NA, nrow(geochron))
    calCurves[which(geochron$cc == 0)] = "normal"
    calCurves[which(geochron$cc == 1)] = "intcal13"
    
    test = Bchronology(ages=geochron$age,
                          ageSds=geochron$error, 
                          calCurves=calCurves,
                          positions=geochron$depth, 
                          positionThicknesses=rep(4,nrow(geochron)),
                          ids=geochron$labid, 
                          predictPositions=depths)
    
    summary(test, type='convergence')
    pdf(paste0('.', "/Cores/", site_params$handle, "/", site_params$handle, "_bchron.pdf"))
    plot(test,
         main=site_params$handle,
         xlab='Age (cal years BP)',
         ylab='Depth (cm)',
         las=1)
    dev.off()
    
    
    post = data.frame(depths=depths, t(test$thetaPredict))
    write.table(post, paste0('.', "/Cores/", site_params$handle, "/", 
                             site_params$handle, "_bchron_samples.csv"), sep=',', col.names = TRUE, row.names = FALSE)
    
    bacon_params[i,]$success = TRUE
    
  }
}
# }

fnames = list.files('Cores', '*_bchron.pdf', recursive=TRUE)

fname_str = sapply(fnames, function(x) paste0('Cores/', x))
fname_str = paste(fname_str, collapse = ' ')

ver=1
sys_str = paste0("gs -sDEVICE=pdfwrite -o bchron_plots_v", ver, ".pdf ", fname_str)
system(sys_str)



