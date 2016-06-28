# library(MASS)
source('utils/calibrate.R')

bacon.params = read.csv('bacon.params.csv', header=TRUE, sep=',')
ncores       = nrow(bacon.params)

get_type_accs <- function(chron, handle){
  
  rate  =  diff(chron$age)/diff(chron$depth)
  depth = chron$depth[1:(nrow(chron)-1)] + diff(chron$depth)/2
  thick = diff(chron$depth)
  age   = chron$age[1:(nrow(chron)-1)] + diff(chron$age)/2
  site  = rep(handle, nrow(chron)-1)
  cc    = rep(unique(chron$cc), nrow(chron)-1)
  
  return(data.frame(site=site, depth=depth, thick=thick, age=age, rate=rate, cc=cc))
}

accs      = list()
core_tops = list()

wmean.date <- function(x)sum(x$ageGrid*x$densities / sum(x$densities))

lead_sites <- data.frame(site=character(0), siteid=numeric(0), max_age=numeric(0), presett=numeric())

mem = data.frame(site=character(0), mem=numeric(0))

for(i in 1:ncores){
  
#   print(i)
  site.params <- bacon.params[i,]
  
  if (site.params$suit){  
    
    chron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE) 
    
    if (any(chron$cc==1)){
      calib = Bchroncal(ages=chron$age[chron$cc==1], ageSds = chron$error[chron$cc==1])
      chron$age[chron$cc==1] <- sapply(calib, wmean.date)
    }
      
    rate  = diff(chron$age)/diff(chron$depth)
    depth = chron$depth[1:(nrow(chron)-1)] + diff(chron$depth)/2
    age   = chron$age[1:(nrow(chron)-1)] + diff(chron$age)/2
    
    rate_diff  = diff(rate)
    rate_means = (rate[1:(length(rate)-1)] + rate[2:length(rate)])*0.5
    d2         = depth[1:(length(depth)-1)] + diff(depth)/2
    
    mem = 1 - (rate_diff/rate_means)^2
    
    if (length(age_diff) > 1){
      if (!is.na(cor(depth, rate))){
        mem_mean = rbind(mem_mean, c(site=site.params$handle, mem=))
        mem_all  = rbind(mem, c(site=site.params$handle, mem=cor(depth, rate)))
      }
    }
  }
}

mem