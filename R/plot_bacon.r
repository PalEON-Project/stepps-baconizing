
source('R/config.r')

# pollen_thick <- read.csv(paste0('../stepps-baconizing/data/pollen_meta_thick_v6.csv'), header=TRUE, stringsAsFactors=FALSE, sep=',')
# 
# # match with previous thicknessnes to get started (or redo all)
# pollen_meta$thick = pollen_thick$thick[match(pollen_meta$id, pollen_thick$id)]
# pollen_meta$bacon = site_data$bacon[match(pollen_meta$id, site_data$dataset_id)]
# write.csv(pollen_meta, paste0('data/pollen_meta_v', version, '.csv'))
# write.csv(pollen_meta, paste0('data/pollen_meta_thick_v', version, '.csv'))
# 
pollen_meta = read.csv(paste0('data/pollen_meta_thick_v', version, '.csv'))

# fix this!!!
# compile all individual pdfs into one pdf
fnames = pollen_meta$handle
thick = pollen_meta$thick
# meta = data.frame(fname=fnames, thick=thicks[which(bacon_params$suit == 1),'thick'])
# fname_str = sapply(cbind(fnames, thick), function(x) paste0('Cores/', x, '/', x, '_', ,'.pdf'))

fnames_all = list.files(path = "Cores", pattern = ".*.pdf", all.files = TRUE,
                        full.names = FALSE, recursive = TRUE, include.dirs = TRUE)
fnames_all = paste0('Cores/', fnames_all)


fname_str = apply(data.frame(as.vector(fnames), thick), 1, function(x) paste0('Cores/', x[1], '/', x[1], '_', x[2],'.pdf'))
fname_str = fname_str[fname_str %in% fnames_all]
fname_str = paste(fname_str, collapse = ' ')

sys_str = paste0("gs -sDEVICE=pdfwrite -o bacon_plots_v", version, ".pdf ", fname_str)
system(sys_str)

fnames_all = list.files(path = "Cores", pattern = ".*.csv", all.files = TRUE,
                        full.names = FALSE, recursive = TRUE, include.dirs = TRUE)
fnames_all = paste0('Cores/', fnames_all)
fname_str = apply(data.frame(as.vector(fnames), thick), 1, function(x) paste0('Cores/', x[1], '/', x[1], '_', x[2],'_samples.csv'))
fname_str = fname_str[fname_str %in% fnames_all]


fname_sub = strsplit(fname_str, "\\/|\\ ")
fname_sub = paste(unlist(lapply(fname_sub, function(x) x[[3]])), collapse = ' ')

fname_str = paste(fname_str, collapse = ' ')

sys_str = paste0("cp ", fname_str, " ", 'output/')
system(sys_str)
# file.copy(fname_str, )

zip("output", files=file.path('output', list.files('output')))
