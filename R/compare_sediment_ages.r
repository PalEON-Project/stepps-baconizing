#################################################################################################################################################
## compare ages
#################################################################################################################################################
version = 1
pollen_ts <- readr::read_csv(file = paste0('data/sediment_ages_v', version, '.0_varves.csv'))

pollen_ages <- pollen_ts %>% 
  dplyr::select(id, sitename, age_bacon, age_bchron, age_default) %>% 
  filter(!id == 3131 & sitename %in% unique(gsub(" ", "", new.chrons[,1])))

pollen_ages$diff_bacon <- pollen_ages$age_bacon - pollen_ages$age_default
pollen_ages$diff_bcron <- pollen_ages$age_bchron - pollen_ages$age_default

long_df <- melt(pollen_ages, id.vars = c("id", "sitename", "age_default"),
                measure.vars = c("diff_bacon", "diff_bcron"))
