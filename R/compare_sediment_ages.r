#################################################################################################################################################
## compare ages
#################################################################################################################################################
version = 1
pollen_ts = read.csv(file=paste0('data/sediment_ages_', version, '.csv'))

# this one has varve ages labelled as depths
pollen_ts = pollen_ts[!(pollen_ts$id %in% c(3131)),]


plot(pollen_ts$age_bacon, pollen_ts$age_bchron)
plot(pollen_ts$age_bacon, pollen_ts$age_default)
plot(pollen_ts$age_bchron, pollen_ts$age_default)

plot(pollen_ts$age_default, pollen_ts$age_bchron-pollen_ts$age_bacon)
plot(pollen_ts$age_default, pollen_ts$age_default-pollen_ts$age_bacon)
plot(pollen_ts$age_default, pollen_ts$age_default-pollen_ts$age_bchron)

plot(pollen_ts$age_bacon, pollen_ts$age_bchron-pollen_ts$age_bacon)
plot(pollen_ts$age_bacon, pollen_ts$age_default-pollen_ts$age_bacon)
plot(pollen_ts$age_bacon, pollen_ts$age_default-pollen_ts$age_bchron)
