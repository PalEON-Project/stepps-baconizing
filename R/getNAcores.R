#  Pull all North American Sites:
library(Bchron)
library(neotoma)
library(plyr)
library(ggplot2)
library(gridExtra)

if('all.downloads.RData' %in% list.files('data/output')){
  gpid.table <- get_table("GeoPoliticalUnits")
  
  na.sites <- match(c('Canada', 'United States', 'Mexico'), gpid.table$GeoPoliticalName)
  
  na.datasets <- llply(gpid.table$GeoPoliticalID[na.sites], function(x)get_dataset(datasettype = 'pollen', gpid = x))
  
  all.datasets <- bind(bind(na.datasets[[1]], na.datasets[[2]]), na.datasets[[3]])
  
  load('data/output/all.downloads.RData')
} else{
  gpid.table <- get_table("GeoPoliticalUnits")
  
  na.sites <- match(c('Canada', 'United States', 'Mexico'), gpid.table$GeoPoliticalName)
  
  na.datasets <- llply(gpid.table$GeoPoliticalID[na.sites], function(x)get_dataset(datasettype = 'pollen', gpid = x))
  
  all.datasets <- bind(bind(na.datasets[[1]], na.datasets[[2]]), na.datasets[[3]])
  
  all.downloads <- get_download(all.datasets)
  
  save(all.downloads, file = 'data/output/all.downloads.RData')
}
