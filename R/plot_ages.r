

ages    = read.csv('../stepps-data/data/bacon_ages/pollen_ts_bacon_v8.csv')
markers = read.csv('data/all_markers_v5.csv')

# age_bacon   = ages
# age_default = 

aggregate(geo_age~dataset_id, data=markers,  FUN = function(x) length(unique(x)))

plot(ages$age_default, ages$age_default-ages$age_bacon)

age_diff = ages$age_default-ages$age_bacon
foo=ages[age_diff>5000,]
foo[,1:5]


# colour by number of chron controls WE use