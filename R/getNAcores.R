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
  
  all.datasets <- do.call(bind, na.datasets)
  
  load('data/output/all.downloads.RData')
} else{
  gpid.table <- get_table("GeoPoliticalUnits")
  
  na.sites <- match(c('Canada', 'United States', 'Mexico'), gpid.table$GeoPoliticalName)
  
  na.datasets <- llply(gpid.table$GeoPoliticalID[na.sites], function(x)get_dataset(datasettype = 'pollen', gpid = x))
  
  all.datasets <- do.call(bind, na.datasets)
  
  all.downloads <- get_download(all.datasets)
  
  save(all.downloads, file = 'data/output/all.downloads.RData')
}
