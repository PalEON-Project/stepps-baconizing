require(neotoma)
require(plyr)
require(raster)
library(Bchron)

source('R/config.r')
source('R/utils/helpers.r')
source('R/utils/compile_lists.r')

# load list containing pollen counts
pollen_meta <- read.csv('data/pollen_meta_thick_v7.csv', header=TRUE, stringsAsFactors=FALSE, sep=',')
# pollen_meta = pollen_meta[pollen_meta$bacon == TRUE,]

# pollen_meta <- read.csv('data/pollen_meta_2014-07-22.csv', header=TRUE)
# pollen_meta <- get_survey_year(pollen_meta)
# 
# # for some reason the datasetID changed for Jones Lake
# pollen_meta[pollen_meta$datasetID == 1394, 'datasetID'] = 15274

# read in dictionaries
pollen.equiv.stepps = read.csv("../stepps-data/pollen.equiv.stepps.csv", stringsAsFactors=F)
pollen.equiv    = read.csv("../stepps-data/pollen.equiv.csv", stringsAsFactors=F, sep=',', row.names=NULL)

ids = pollen_meta$id

pol=list()
# load list containing pollen counts
# will load object called pol
# the first time takes a while to pull from Neotoma
if (file.exists(paste0('data/pol_stepps_', my_date, '.rdata'))) {
  # loads object pol
  load(paste0('data/pol_stepps_', my_date, '.rdata'))
} 
if (!file.exists(paste0('data/pol_stepps_', my_date, '.rdata'))|(length(ids)!=length(pol))){
  
  nsites = length(ids)
  
  # download and save the raw data
  pol_raw = list()
  for (i in 1:nsites){ 
    id = as.numeric(ids[i])
    print(id)
    # if (id == 1394) id = 15274
    pol_raw[[i]] = get_download(id)
  }  
  save(pol_raw, file=paste0('data/pol_', Sys.Date(), '.rdata'))
  
  
  # miss = list()
  # aggregate the taxa
  pol = list()
  for (i in 1:nsites){  
    print(i)
    convert1 = compile_list_neotoma(pol_raw[[i]][[1]], 'Stepps', pollen.equiv)
    
    #     out = compile_list_neotoma(pol_raw[[i]][[1]], 'Stepps')
    #     convert1 = out[[1]]
    #     miss[[i]] = out[[2]]
    
    #pol[[i]] = compile_list_stepps(convert1, list.name='all', pollen.equiv.stepps, cf = TRUE, type = TRUE)
    pol[[i]] = compile_list_stepps(convert1, list.name='must_have', pollen.equiv.stepps, cf = TRUE, type = TRUE)
    pol[[i]]$dataset$site.data$state = pollen_meta$state2[i]
  }
  
  save(pol, file=paste0('data/pol_stepps_', Sys.Date(), '.rdata'))
  
} 

# bacon.params <- read.csv('bacon.params.csv', header=TRUE, sep=',')
# thicks = read.csv('bacon.fit.thick.csv')$thick

# find min and max geochron and pollen ages
ncores = nrow(pollen_meta)
age_con = data.frame(id=numeric(0), 
                     handle=character(0), 
                     geo_age_max=numeric(0), 
                     geo_age_min=numeric(0),
                     pol_age_max=numeric(0),
                     pol_age_min=numeric(0)) 
for (i in 1:ncores){
  print(i)
  
  if (is.na(pollen_meta[i, 'bacon'])){
    next
  }
  
  handle = pollen_meta[i,'handle']
  id     = pollen_meta[i,'id']
  thick  = pollen_meta[i,'thick']
  # find hiatus depth
  if (pollen_meta[i, 'bacon'] == TRUE){
    geo_samples = read.table(paste0('Cores/', handle, '/', handle, '_', thick, '_geo_samples.csv'), sep=',', header=TRUE)

    geo_age_min = mean(as.numeric(geo_samples[1, 2:ncol(geo_samples)]))
    geo_age_max = mean(as.numeric(geo_samples[nrow(geo_samples), 2:ncol(geo_samples)]))
    
    pol_samples = read.table(paste0('Cores/', handle, '/', handle, '_', thick, '_samples.csv'), sep=',', header=TRUE)
    
    pol_age_min = mean(as.numeric(pol_samples[1, 2:ncol(pol_samples)]))
    pol_age_max = mean(as.numeric(pol_samples[nrow(pol_samples), 2:ncol(pol_samples)]))
    
    age_con = rbind(age_con, data.frame(id=id, 
                               handle=handle, 
                               geo_age_max=geo_age_max, 
                               geo_age_min=geo_age_min, 
                               pol_age_max=pol_age_max, 
                               pol_age_min=pol_age_min))
  }  
}

# age_con = age_con[age_con$id != 1004,]

write.table(age_con, file=paste0('data/pol_ages_bacon_v', version, '.csv'), quote=FALSE, row.names=FALSE, sep=',')


# find min and max geochron and pollen ages
ncores = nrow(pollen_meta)
age_con = data.frame(id=numeric(0), 
                     handle=character(0), 
                     geo_age_max=numeric(0), 
                     geo_age_min=numeric(0),
                     pol_age_max=numeric(0),
                     pol_age_min=numeric(0)) 
for (i in 1:ncores){
  print(i)
  
  if (is.na(pollen_meta[i, 'bacon'])){
    next
  }
  
  handle = pollen_meta[i,'handle']
  id     = pollen_meta[i,'id']
  thick  = pollen_meta[i,'thick']
  # find hiatus depth
  if (pollen_meta[i, 'bchron'] == TRUE){
    geo_samples = read.table(paste0('Cores/', handle, '/', handle, '_bchron_geo_samples.csv'), sep=',', header=TRUE)
    
    geo_age_min = mean(as.numeric(geo_samples[1, 2:ncol(geo_samples)]))
    geo_age_max = mean(as.numeric(geo_samples[nrow(geo_samples), 2:ncol(geo_samples)]))
    
    pol_samples = read.table(paste0('Cores/', handle, '/', handle, '_bchron_samples.csv'), sep=',', header=TRUE)
    
    pol_age_min = mean(as.numeric(pol_samples[1, 2:ncol(pol_samples)]))
    pol_age_max = mean(as.numeric(pol_samples[nrow(pol_samples), 2:ncol(pol_samples)]))
    
    age_con = rbind(age_con, data.frame(id=id, 
                                        handle=handle, 
                                        geo_age_max=geo_age_max, 
                                        geo_age_min=geo_age_min, 
                                        pol_age_max=pol_age_max, 
                                        pol_age_min=pol_age_min))
  }  
}

# age_con = age_con[age_con$id != 1004,]

write.table(age_con, file=paste0('data/pol_ages_bchron_v', version, '.csv'), quote=FALSE, row.names=FALSE, sep=',')


############################################################################################################################################
## ages of pre-settlement sample
############################################################################################################################################
# find depth span to determine if there is a minimum number of thicknesses that work
ncores = nrow(pollen_meta)
age_ps = data.frame(id=numeric(0), 
                     handle=character(0), 
                     type = character(0),
                     depth_ps = numeric(0),
                     geo_age_ps=numeric(0), 
                     set_year_default=numeric(0),
                     x=numeric(0),
                     y=numeric(0)) 
amb = rep(FALSE, ncores)
for (i in 1:ncores){
  print(i)
  
  if (is.na(pollen_meta[i, 'bacon'])){
    next
  }
  
  
  handle = pollen_meta[i,'handle']
  id     = pollen_meta[i,'id']
  thick  = pollen_meta[i,'thick']
  # find hiatus depth
  if (pollen_meta[i, 'bacon'] == TRUE){
    labids = read.table(paste0('Cores/', handle, '/', handle, '.csv'), sep=',', header=TRUE)[,1]
    
    if (any(substr(labids, 1, 4) %in% c('Pres', 'Ambr', 'Sett'))){
      # if (any(substr(labids, 1, 4) %in% c('Presettlement_paleon', 'Ambrosia rise', 'Settlement'))){
      # idx = which(labids %in% c('Presettlement_paleon', 'Ambrosia rise', 'Settlement'))
      
      idx = which(substr(labids, 1, 4) %in% c('Pres', 'Ambr', 'Sett'))
      
      print(idx)
      amb[i] = TRUE
      
      geo_samples = read.table(paste0('Cores/', handle, '/', handle, '_', thick, '_geo_samples.csv'), sep=',', header=TRUE)
      
      depth_ps = geo_samples[idx, 'depths']
      # geo_age_ps = mean(as.numeric(geo_samples[idx, 2:ncol(geo_samples)]))
      geo_age_ps = quantile(as.numeric(geo_samples[idx, 2:ncol(geo_samples)]), probs=c(0.025, 0.5, 0.975))
      
      
      # chron_control = get_chroncontrol(id)
      # Sys.sleep(3)
      
      # idx_def = which(substr(chron_control[[1]]$control.type, 1, 4) %in% c('Pres', 'Ambr', 'Sett'))
      # if (length(idx>0)){
      #   set_year_default=chron_control[[1]][idx_def,'age'] 
      # } else {
      #   set_year_default = NA
      # }
      
      # need to fix this
      if (any(pol[[i]]$sample.meta$depth == geo_samples$depth[idx])){
        set_age_default=pol[[i]]$sample.meta$age[which(pol[[i]]$sample.meta$depth == geo_samples$depth[idx])]
      } else {
        set_year_default = NA
      }
      
      age_ps = rbind(age_ps, data.frame(id=id, 
                                        handle=handle, 
                                        type=as.vector(labids[idx]),
                                        depth_ps = as.vector(depth_ps),
                                        geo_age_ps=geo_age_ps[2], 
                                        geo_age_lb=geo_age_ps[1], 
                                        geo_age_ub=geo_age_ps[3], 
                                        set_age_default=set_age_default,
                                        x=pollen_meta[i,'x'], 
                                        y=pollen_meta[i,'y']))
    }
  }  
}

par(mfrow=c(1,1))
hist(age_ps$geo_age_ps)

summary(age_ps$geo_age_ps)
age_ps[which(age_ps$geo_age_ps > 1000),]

# age_ps = age_ps[age_ps$id != 1004,]

age_ps$set_year = 1950 - pollen_meta$set.year[match(age_ps$id, pollen_meta$id)]

write.table(age_ps, file=paste0('data/pol_age_ps_v', version, '.csv'), quote=FALSE, row.names=FALSE, sep=',')

library(ggplot2)

pdf('figures/settlement_ages.pdf')
plot(age_ps$geo_age_ps, age_ps$set_year, pch=19, xlab='Bacon', ylab='PLS (Bacon inputs)', ylim=c(0,200))
abline(a=0, b=1, col='grey', lty=2)
dev.off()


plot(age_ps$set_age_default, age_ps$set_year, pch=19, xlab='Neotoma', ylab='PLS (Bacon inputs)', ylim=c(0,200))
abline(a=0, b=1, col='grey', lty=2)

ggplot(data=age_ps) + geom_point(data=age_ps, aes(y=age_ps$geo_age_ps, x=age_ps$set_year)) + 
  # geom_point(data=age_ps, aes(y=age_ps$geo_age_ps, x=age_ps$set_age_default), colour='blue') + 
  geom_errorbar(aes(x=age_ps$set_year, ymin=age_ps$geo_age_lb, ymax=age_ps$geo_age_ub)) + 
  ylab('Bacon age') + xlab('Settlement year') +coord_cartesian(ylim=c(-100,500))

library(reshape2)
library(tidyr)
library(GGally)

age_ps_melt=melt(age_ps, measure.vars=c("set_year", "set_age_default", "geo_age_ps"))
levels(age_ps_melt$variable) = c("PalEON", "Neotoma", "Bacon")
age_ps_2=dcast(age_ps_melt, id~variable, value.var="value")

# get rid of disterhaft
age_ps_2 = age_ps_2[which(age_ps_2$Neotoma<900),]

age_ps_2[which(age_ps_2$Bacon>400),]

png(file='figures/settlement_ages.png')
ggpairs(age_ps_2[,2:4])
dev.off()

# 
# ggplot(data=age_ps_melt) + geom_point(aes(x=age_ps_melt$value, y=age_ps_melt$value)) + facet_grid(variable~variable)
# 
# 
# ggplot(data=age_ps) + geom_histogram(aes(geo_age_ps, binwidth=10))
# 
# ggplot(data=age_ps) + geom_histogram(aes(set_year, binwidth=10))
# 
# hist(age_ps$geo_age_ps, breaks=20)

############################################################################################################################################
## ages of all markers
############################################################################################################################################
# find depth span to determine if there is a minimum number of thicknesses that work
ncores = nrow(pollen_meta)
age_markers = data.frame(id=numeric(0), 
                         handle=character(0), 
                         type = character(0),
                         depth = numeric(0),
                         geo_age=numeric(0), 
                         x=numeric(0),
                         y=numeric(0)) 
amb = rep(FALSE, ncores)
for (i in 1:ncores){
  print(i)
  
  if (is.na(pollen_meta[i, 'bacon'])){
    next
  }
  
  handle = pollen_meta[i,'handle']
  id     = pollen_meta[i,'id']
  thick  = pollen_meta[i,'thick']
  # find hiatus depth
  if (pollen_meta[i, 'bacon'] == TRUE){
    labids = read.table(paste0('Cores/', handle, '/', handle, '.csv'), sep=',', header=TRUE)[,1]
#     
#     geo_samples = read.table(paste0('Cores/', handle, '/', handle, '_', thick, '_geo_samples.csv'), sep=',', header=TRUE)
#     
    # labids = geo_samples[,1]
    
    if (any(substr(labids, 1, 4) %in% c('Pres', 'Ambr', 'Sett'))){
      amb[i] = TRUE
    }
    
    geo_samples = read.table(paste0('Cores/', handle, '/', handle, '_', thick, '_geo_samples.csv'), sep=',', header=TRUE)
    
    depth   = geo_samples[, 1]
    geo_age = rowMeans(geo_samples[, 2:ncol(geo_samples)])
    
    age_markers = rbind(age_markers, data.frame(id=id, 
                                                handle=handle, 
                                                type=labids,
                                                depth = depth,
                                                geo_age=geo_age, 
                                                x=rep(pollen_meta[i,'x'], length(labids)),
                                                y=rep(pollen_meta[i,'y'], length(labids))))
  }  
}

age_markers = age_markers[age_markers$id != 1004,]

version=1
write.table(age_markers, file=paste0('data/bacon_age_markers_v', version, '.csv'), quote=FALSE, row.names=FALSE, sep=',')


############################################################################################################################################
## plot ages
############################################################################################################################################

age_con$stat_id = seq(1, nrow(age_con))

ggplot(data=age_con) + 
  geom_segment(aes(x=pol_age_min,y=stat_id, xend=pol_age_max, yend=stat_id)) + 
  geom_point(aes(x=geo_age_max,y=stat_id ), colour='red') + 
  geom_point(aes(x=geo_age_min,y=stat_id ), colour='blue')


















# drop the pollen samples that are 500 years older than the oldest geochron date
constrain_pollen <- function(pollen, age_con, nbeyond=1000){
  
  drop_samples = vector(length=nrow(pollen))
  for (i in 1:nrow(pollen)){
    
    idx = match(pollen$id[i], age_con$id)
    
    if (is.na(idx)){
      print(paste0(i, ' WTF'))
      print(pollen$id[i])
    }
    
    drop_samples[i] = pollen$age_bacon[i] > (age_con$geo_age_max[idx] + nbeyond)
  }
  
  return(drop_samples)
}

lost_samps = data.frame(nbeyond=numeric(0), dropped=numeric(0), nsites=numeric(0))
coords_drop = data.frame(nbeyond=numeric(0), lat=numeric(0), long=numeric(0))
for (d in seq(500,5000, by=500)){
  drop_samples = constrain_pollen(pollen, age_con, nbeyond=d)
  lost_samps = rbind(lost_samps, data.frame(nbeyond=d, dropped=sum(drop_samples), nsites=length(unique(pollen[drop_samples, 'id']))))
  
  coords_drop = rbind(coords_drop, data.frame(nbeyond=rep(d, sum(drop_samples)), lat=pollen[drop_samples, 'lat'], long=pollen[drop_samples, 'long']))
}

pollen_to_albers <- function(pollen_ts){
  
  centers_pol = data.frame(x=pollen_ts$long, y=pollen_ts$lat)
  
  coordinates(centers_pol) <- ~ x + y
  proj4string(centers_pol) <- CRS('+proj=longlat +ellps=WGS84')
  
  centers_polA <- spTransform(centers_pol, CRS('+init=epsg:3175'))
  centers_polA <- as.matrix(data.frame(centers_polA))/1000000
  
  pollen_ts$long = centers_polA[,'x']
  pollen_ts$lat = centers_polA[,'y']
  
  colnames(pollen_ts)[grep("lat", colnames(pollen_ts))] = 'y'
  colnames(pollen_ts)[grep("long", colnames(pollen_ts))] = 'x'
  
  return(pollen_ts)
}

coords_drop = pollen_to_albers(coords_drop)


pdf(file=paste0('figures/constraint.pdf'))
par(mfrow=c(1,1))
plot(lost_samps[,1], lost_samps[,2], pch=19, xlab='Years beyond last geochron marker', ylab='Number lost (samples and sites)')
points(lost_samps[,1], lost_samps[,3], pch=19, col='blue')
dev.off()

library(gridExtra)
library(maptools)

us.shp <- readShapeLines('../stepps-prediction/data/map_data/us_alb.shp',
                         proj4string=CRS('+init=epsg:3175'))
us.shp@data$id <- rownames(us.shp@data)
us.fort <- fortify(us.shp, region='id') 

limits = readRDS('data/limits.rds')
rescale=1e6

add_map_albers <- function(plot_obj, map_data=us.fort, limits){
  p <- plot_obj + geom_path(data=map_data, aes(x=long, y=lat, group=group),  colour='grey55') + 
    #     scale_x_continuous(limits = c(min(umw.coord$x, na.rm=TRUE), max(umw.coord$x, na.rm=TRUE))) +
    #     scale_y_continuous(limits = c(min(umw.coord$y, na.rm=TRUE), max(umw.coord$y, na.rm=TRUE)))#, colour = "black", size = 1, fill = "white", aes(x=long, y=lat, group = group))
    # #   
    #     scale_x_continuous(limits = c(min(dat[,1], na.rm=TRUE), max(dat[,1], na.rm=TRUE))) +
    #     scale_y_continuous(limits = c(min(dat[,2], na.rm=TRUE), max(dat[,2], na.rm=TRUE)))
    scale_x_continuous(limits = limits$xlims*1000000) +
    scale_y_continuous(limits = limits$ylims*1000000) #+ coord_map("albers")
  return(p)
  
}


p <- ggplot(data=coords_drop) + geom_point(data=coords_drop, aes(x=x*1e6, y=y*1e6), shape=19) + #, colour='#FF6600', shape=19) + 
  coord_fixed() #+ scale_x_continuous(limits$xlims) + scale_y_continuous(limits$ylims)
p <- add_map_albers(p, map_data=us.fort, limits)#, xlims=c(xlo,xhi), ylims=c(ylo, yhi))
p <- p + facet_grid(nbeyond~.)
p <- p + facet_wrap(~nbeyond, nrow=2)
#   p <- p + theme(strip.text.x = element_blank(),
#                 strip.text.y = element_blank())
#   p <- p + theme(strip.background = element_blank())
p <- theme_clean(p) + theme(strip.text.y = element_text(size = rel(1.5)), 
                            strip.text.x = element_text(size = rel(1.5)))
print(p)
ggsave('figures/constraint_maps.pdf')

# max_pol_age = sapply(pollen_dat, function(x) max(x$sample.meta$age))
# sum(max_pol_age > 2000)
# 
# ncores = length(pollen_dat)
# 
# chronID   <- sapply(pollen_dat, function(x)try(x$sample.meta$chronology.id[1]))
# datasetID <- sapply(pollen_dat, function(x)try(x$dataset$dataset.meta$dataset.id))
# nsamples  <- sapply(pollen_dat, function(x)try(nrow(x$sample.meta)))
# 
# chron_tables <- lapply(chronID, function(x)try(get_chroncontrol(x)))
# 
# aa <- ldply(1:length(chron_tables),
#             .fun=function(x){
#               if(length(chron_tables[[x]])>1){
#                 nages = length(chron_tables[[x]]$chron.control$age)
#                 output<- data.frame(datasetID = rep(datasetID[x], nages),
#                                     chronologyID = rep(chronID[x], nages),
#                                     site = rep(pollen_dat[[x]]$dataset$site.data$site.name, nages),
#                                     type = chron_tables[[x]]$chron.control$control.type,
#                                     age = chron_tables[[x]]$chron.control$age,
#                                     age.sd =  as.numeric(chron_tables[[x]]$chron.control$age.old) - 
#                                       as.numeric(chron_tables[[x]]$chron.control$age),
#                                     depth = chron_tables[[x]]$chron.control$depth,
#                                     thick = chron_tables[[x]]$chron.control$thickness,
#                                     agemodel = rep(chron_tables[[x]]$meta$age.model, nages))
#               }
#               else{
#                 output <- data.frame(site = NA, type = NA, age = NA)
#               }
#             })
# 
# 
# write.table(aa, file='data/marker_types.csv', sep=',', row.names=F)
# 
# 
# 
# 
# 
# markers = read.csv('data/marker_types.csv', sep=',', header=TRUE)
# 
# marker_types = data.frame(type=as.vector(unique(markers$type)), code=seq(1, length(unique(markers$type))))
# markers$code = marker_types[match(marker$type, marker_types$type), 'code']
# 
# 
# num_sites <- function(x) {
#   length(unique(x))
# }
# 
# type_sum = aggregate(site ~ type, markers,  strat_types)
# write.table(type_sum, file='data/marker_types_summary.csv', sep=',', row.names=TRUE)
# 
# 
# older_2000 <- function(x){
#   any(x>2000)
# }
# 
# old_markers = aggregate(age ~ site + code, markers,  older_2000)
# old_sub = old_markers[which((old_markers$code %in% c(3, 7, 10, 15)) & (old_markers$age == TRUE)),]
# unique(old_sub$site)
# 
# 
# 
# 
# 
# rc_oldest = sapply(chron_tables, function(x) max(x$chron.control$age[x$chron.control$control.type %in% marker_types[c(3, 7, 10, 15),1]]))
# 
# core_ma=data.frame(id=ids, rc=rc_oldest, pol=max_pol_age)
# core_ma$rc[core_ma$rc == -Inf] = 0
# 
# bchron_ages = BchronCalibrate(ages      = core_ma$rc[core_ma$rc >0],
#                 ageSds    = rep(100, length(core_ma$rc[core_ma$rc >0])), 
#                 calCurves = rep('intcal13', length(core_ma$rc[core_ma$rc >0])))
# 
# bchron_ages2 = sapply(bchron_ages, function(x) x$ages)
# 
# 
# core_ma$rc_ybp[core_ma$rc >0] = BchronCalibrate(ages      = core_ma$rc[core_ma$rc >0],
#                                                 ageSds    = rep(100, length(core_ma$rc[core_ma$rc >0])), 
#                                                 calCurves = rep('intcal13', length(core_ma$rc[core_ma$rc >0])))
# 
# sum(core_ma$rc >0)
# 
# core_ma$rc[core_ma$rc >0] - core_ma$pol[core_ma$rc >0]
# 
# back = apply(core_ma[core_ma$rc >0,], 1, min)
# sum(back>2000)
# 
# 
# plot(core_ma$rc, core_ma$pol)
