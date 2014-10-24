pollen_to_albers <- function(coord, rescale=1e6){
  
  coordinates(coord) <- ~ long + lat
  proj4string(coord) <- CRS('+proj=longlat +ellps=WGS84')
  
  coordA <- spTransform(coord, CRS('+init=epsg:3175'))
  coordA <- as.matrix(data.frame(coordA))/rescale
  
  colnames(coordA) <- c('x', 'y')
  
  return(coordA)
}

get_survey_year <- function(pollen_meta){
  
  pls <- raster('../data/pls/age_of_sample.tif')
#   plot(pls)

  coord  = data.frame(long=pollen_meta$long, lat= pollen_meta$lat)
  coordA = pollen_to_albers(coord, rescale=1)

  set.year <- extract(pls, coordA)
  
  set.year[is.na(set.year) & pollen_meta$state == 'michigan:north'] = 1860
  set.year[is.na(set.year) & pollen_meta$state == 'michigan:south'] = 1840
  
  pollen_meta <- cbind(pollen_meta, coordA)
  pollen_meta$set.year <- set.year
  
  return(pollen_meta)
}

set_lead_error <- function(chron, types){
  
  x = c(10, 100, 150)
  y = c(1.5, 15, 85)
  model <- lm(log(y)~x)
  
  lead.cond <- types == 'Lead-210'
  
  ages = data.frame(x = chron[lead.cond & (chron[lead.cond, 'error'] == 0), 'age'])
  chron[lead.cond & (chron[lead.cond, 'error'] == 0), 'error'] = ceiling(exp(as.vector(predict(model, ages)))/2)
  
  print(paste0("Some or all of the lead-210 dates in core ", corename, " have zero error. Adjusting."))
  
  return(chron)
}

bacon_age_posts <- function(d, b.depths, out, thick)
{ 
  its=out[,1:(ncol(out)-1)]
  
  elbows <- cbind(its[,1])
  accs <- its[,2:(ncol(its)-1)]
  for(i in 2:ncol(accs))
    elbows <- cbind(elbows, elbows[,ncol(elbows)] + (thick * accs[,i-1]))
  
  if (d %in% b.depths)
    ages <- elbows[,which(b.depths == d)] 
  else
    {
      maxd <- max(which(b.depths < d))
      ages <- elbows[,maxd] + ((d-b.depths[maxd]) * accs[,maxd])
    }
  ages
}


run.bacon <- function(site.params){
  
  source('Bacon.R')
  
  # check for suitability
  if (site.params$suit==1){
    
    # find hiatus depth
    geochron = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)  
      
    if (any(substr(geochron$labid, 1, 4) == 'Pres') & nrow(geochron) > 2){
      
      if (which(substr(geochron$labid, 1, 4) == 'Pres') == nrow(geochron)){
        hiatus.depth       = NA
        acc.mean.val       = site.params$acc.mean.mod
        acc.shape.val      = site.params$acc.shape.mod      
        site.params$hiatus = 0
      } else if (which(substr(geochron$labid, 1, 4) == 'Pres') == 1){
        hiatus.depth       = NA
        acc.mean.val       = site.params$acc.mean.old
        acc.shape.val      = site.params$acc.shape.old      
        site.params$hiatus = 0
      } else {    
        hiatus.depth = geochron$depth[substr(geochron$labid, 1, 4) == 'Pres'] #- 1
        acc.mean.val     = c(site.params$acc.mean.mod, site.params$acc.mean.old)
        acc.shape.val    = c(site.params$acc.shape.mod, site.params$acc.shape.old)
        site.params$hiatus = 1
      }
      
    } else if (any(substr(geochron$labid, 1, 4) == 'Pres') & nrow(geochron) == 2) {
      hiatus.depth       = NA
      acc.mean.val       = site.params$acc.mean.mod
      acc.shape.val      = site.params$acc.shape.mod      
      site.params$hiatus = 0
    } else if (!any(substr(geochron$labid, 1, 4) == 'Pres')) {
      hiatus.depth       = NA
      acc.mean.val       = site.params$acc.mean.old
      acc.shape.val      = site.params$acc.shape.old      
      site.params$hiatus = 0
    } 
        
    out <- try(
      with(site.params, 
           Bacon(handle, 
                 acc.mean      = acc.mean.val, 
                 acc.shape     = acc.shape.val,
                 #                  acc.shape     = acc.shape.val,
                 mem.strength  = mem.strength,
                 mem.mean      = mem.mean,
                 thick         = thick,
                 ask           = FALSE,
                 suggest       = FALSE,
                 depths.file   = FALSE, # i want to pass one, but bacon sometimes barfs if i do and i can't figure out why
                 hiatus.shape  = 1,
                 hiatus.mean   = 10,
                 hiatus.depths = hiatus.depth)
      )
    )
    if (!(class(out) == 'try-error')){
      
      depths       = scan(sprintf('Cores/%s/%s_depths.txt', site.params$handle, site.params$handle))
      depths.bacon = scan(sprintf('Cores/%s/%s_bacon_depths.csv', site.params$handle, site.params$handle))
      
      ndepths = length(depths.bacon)
      
      core.path = sprintf('Cores/%s', site.params$handle)
      #       outfile  = Sys.glob(paste0(core.path, '_*.out')) 
      outfile = paste0(core.path, '/', site.params$handle, '_', ndepths, '.out')
      output = read.table(outfile)
      
      #       # build depths.bacon using thick and upper and lower depths from 
      #       marker.depths = read.table(sprintf('Cores/%s/%s.csv', site.params$handle, site.params$handle), sep=',', header=TRUE)$depth
      #       depths.bacon = seq(min(marker.depths), max(marker.depths), by=thickness)
      #       if (!is.null(hiatus.depth)) ndepths = ndepths + 1
      
      #       # loses connection when in a loop for some reason...
      #       core_files = list.files(sprintf('Cores/%s/', site.params$handle))
      #       out_file   = core_files[grep("*.out", core_files)]
      #       output     = read.table(sprintf('Cores/%s/%s', site.params$handle, out_file))
      
      #       output = read.table(sprintf('Cores/%s/%s_%d.out', site.params$handle, site.params$handle, length(depths.bacon)))
      
      if ( (min(depths) < min(depths.bacon)) | (max(depths) > max(depths.bacon)) ){
        depths = depths[depths > min(depths.bacon)]
        depths = depths[(depths - max(depths.bacon)) < 100]
      }
      
      iters   = nrow(output)
      samples = matrix(0, nrow = length(depths), ncol = iters)
      colnames(samples) = paste0('iter', rep(1:iters))
      
      for (j in 1:length(depths)){    
        #         samples[j,] = Bacon.Age.d(depths[j]) 
        samples[j,] = bacon_age_posts(d=depths[j], b.depths=depths.bacon, out=output, thick=site.params$thick)
      }
      
      post = data.frame(depths=depths, samples)
      
      write.table(post, paste0('.', "/Cores/", site.params$handle, "/", 
                               site.params$handle, "_samples.csv"), sep=',', col.names = TRUE, row.names = FALSE)
      site.params$success = 1
      
    } else {
      
      site.params$success = 0
    
    }
    
  }
  return(site.params)
}


# build pollen counts
build_core_locs <- function(tmin, tmax, int, pollen_ts){
  
  if (int > 0){
    #   breaks = seq(0,2500,by=int)
    breaks = seq(tmin,tmax,by=int)
    
    meta_pol  = pollen_ts[which((pollen_ts[, 'ages'] >= tmin) & 
                                  (pollen_ts[, 'ages'] <= tmax)),]
    
    meta_agg = matrix(NA, nrow=0, ncol=ncol(meta_pol))
    colnames(meta_agg) = colnames(meta_pol)
    
    ids = unique(meta_pol$id)
    ncores = length(ids)
    
    for (i in 1:ncores){
      
      #print(i)
      core_rows = which(meta_pol$id == ids[i])
      #     core_counts = counts[core_rows,]
      
      for (j in 1:(length(breaks)-1)){
        
        #print(j)
        age = breaks[j] + int/2
        
        age_rows = core_rows[(meta_pol[core_rows, 'ages'] >= breaks[j]) & 
                               (meta_pol[core_rows, 'ages'] < breaks[j+1])]
        
        if (length(age_rows)>=1){
          
          meta_agg      = rbind(meta_agg, meta_pol[core_rows[1],])
          meta_agg$ages[nrow(meta_agg)] = age/100
          
        }
        #         } else if (length(age_rows) == 0){
        #           
        #           meta_agg      = rbind(meta_agg, meta_pol[core_rows[1],])
        #           meta_agg$ages[nrow(meta_agg)] = age/100
        #         }
        
      }
    }
  }
  
  return( meta_agg )
}


# pollen_to_albers <- function(pollen_ts){
#   
#   centers_pol = data.frame(x=pollen_ts$long, y=pollen_ts$lat)
#   
#   coordinates(centers_pol) <- ~ x + y
#   proj4string(centers_pol) <- CRS('+proj=longlat +ellps=WGS84')
#   
#   centers_polA <- spTransform(centers_pol, CRS('+init=epsg:3175'))
#   centers_polA <- as.matrix(data.frame(centers_polA))/1000000
#   
#   pollen_ts$long = centers_polA[,'x']
#   pollen_ts$lat = centers_polA[,'y']
#   
#   colnames(pollen_ts)[grep("lat", colnames(pollen_ts))] = 'y'
#   colnames(pollen_ts)[grep("long", colnames(pollen_ts))] = 'x'
#   
#   return(pollen_ts)
# }

add_map_albers <- function(plot_obj, map_data=us.fort, limits){
  p <- plot_obj + geom_path(data=map_data, aes(x=long, y=lat, group=group),  colour='grey55') + 
    scale_x_continuous(limits = limits$xlims) +
    scale_y_continuous(limits = limits$ylims) #+ coord_map("albers")
  return(p)
  
}

get_limits <- function(centers, buffer){
  xlo = min(centers[,1]) - buffer - 50000
  xhi = max(centers[,1]) + buffer
  
  ylo = min(centers[,2]) - buffer
  yhi = max(centers[,2]) + buffer + 50000
  
  return(list(xlims=c(xlo,xhi),ylims=c(ylo, yhi)))
}  

theme_clean <- function(plot_obj){
  plot_obj <- plot_obj + theme(axis.ticks = element_blank(), 
                               axis.text.y = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               plot.background = element_rect(fill = "transparent",colour = NA))
  
  return(plot_obj)
}
