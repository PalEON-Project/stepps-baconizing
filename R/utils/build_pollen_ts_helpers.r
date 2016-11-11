split_mi <- function(meta, longlat){
  
  #   meta=veg_meta
  if (any(colnames(meta)=='region')){
    meta$state = meta$region
  } 
  
  if (longlat){
    centers_ll = data.frame(x=meta$long, y=meta$lat)
  } else {
    centers = data.frame(x=meta$x, y=meta$y)
    
    coordinates(centers) <- ~x + y
    proj4string(centers) <- CRS('+init=epsg:3175')
    
    centers_ll <- spTransform(centers, CRS('+proj=longlat +ellps=WGS84'))
    centers_ll <- as.matrix(data.frame(centers_ll))
  }
  
  meta$state[meta$state == 'michigan:north'] = 'michigan_north'
  idx.mi = which(meta$state=='michigan_north')
  meta$state2 = as.vector(meta$state)
  meta$state2[idx.mi] = map.where(database="state", centers_ll[idx.mi,1], centers_ll[idx.mi,2])
  idx.na = which(is.na(meta$state2))
  idx.not.na = which(!is.na(meta$state2))
  
  #   plot(meta$x, meta$y)
  #   points(meta$x[meta$state2=='michigan:north'], meta$y[meta$state2=='michigan:north'], col='red', pch=19)
  
  meta$state[meta$state == 'michigan:south'] = 'michigan_south'
  idx.mi.s = which(meta$state=='michigan_south')
  meta$state2[idx.mi.s] = 'michigan:south'#map.where(database="state", centers_ll[idx.mi.s,1], centers_ll[idx.mi.s,2])
  #   points(meta$x[meta$state2=='michigan:south'], meta$y[meta$state2=='michigan:south'], col='blue', pch=19)
  
  #   points(meta$x[meta$state2=='michigan:north'], meta$y[meta$state2=='michigan:north'])
  #   plot(meta$x, meta$y)
  if (length(idx.na)>0){
    for (i in 1:length(idx.na)){
      print(i)
      idx = idx.na[i]
      centers = centers_ll[idx.not.na,]
      dmat = rdist(matrix(centers_ll[idx,], nrow=1) , matrix(centers, ncol=2))
      min.val = dmat[1,which.min(dmat[which(dmat>1e-10)])]
      idx_close = which(dmat == min.val)
      #     print(as.vector(meta$state[idx]))
      #     print(meta$state2[idx])
      state  = map.where(database="state", centers[idx_close,1], centers[idx_close,2])
      #     print(map.where(database="state", centers[idx_close,1], centers[idx_close,2]))
      meta$state2[idx] = state
      #     points(meta$x[idx], meta$y[idx], col='green', pch=19)
    }
  }
  
  meta$state2[which(meta$state2[idx.mi]=='minnesota')] = 'michigan:north'
  
  idx.bad = which((meta$state2=='michigan:north') & (meta$y<8e5))
  meta$state2[idx.bad] = 'michigan:south'
  #   points(meta$x[idx.bad], meta$y[idx.bad], col='pink', pch=19)
  
  return(meta)
  
}

whitmore2stepps <- function(x, taxa){
  x = compile_taxa_stepps(x, list.name='must_have', alt.table=pollen.equiv.stepps, cf = TRUE, type = TRUE)
  
  #if ('Other' %in% )
  x$counts = x$counts[,!(colnames(x$counts)=='Other')]
  
  zero_taxa = taxa[!(taxa %in% colnames(x$counts))]
  add_back   = matrix(0, nrow=nrow(x$counts), ncol=length(zero_taxa))
  colnames(add_back) = zero_taxa
  
  tmp      = cbind(x$counts, add_back)
  x$counts = tmp[, sort(colnames(tmp))]
  
  return(x)
}
