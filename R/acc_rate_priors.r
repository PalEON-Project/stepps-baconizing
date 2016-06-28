###################################################################################################################################################
## Determine accumulation rate prior for the upper midwestern US using chronological controls from the neotoma db
###################################################################################################################################################
library(MASS)
library(ggplot2)

# load Bchron
source('utils/calibrate.R')

# meta data; includes list of sites
bacon_params = read.csv('bacon.params.csv', header=TRUE, sep=',')
ncores       = nrow(bacon_params)

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

wmean_date <- function(x) sum(x$ageGrid*x$densities / sum(x$densities))
lead_sites <- data.frame(site=character(0), siteid=numeric(0), max_age=numeric(0), presett=numeric())

for(i in 1:ncores){
  
  print(i)
  site_params <- bacon_params[i,]
  
  if (site_params$suit){  
    
    if (!(file.exists(sprintf('Cores/%s/%s.csv', site_params$handle, site_params$handle)))) {
     next 
    } 
    
    chron = read.table(sprintf('Cores/%s/%s.csv', site_params$handle, site_params$handle), sep=',', header=TRUE)
    
    if ( any(substr(chron$labid, 1, 4) == 'Core') ){
      print(chron[which(substr(chron$labid, 1, 4) == 'Core'),])
      core_tops = rbind(core_tops, data.frame(site=site_params$handle, chron[which(substr(chron$labid, 1, 4) == 'Core'),]))
    }
    
    if (any(substr(chron$labid,1,4) == 'Lead')){
      presett=FALSE
      if  (any(substr(chron$labid,1,3) == 'Pre')){presett=TRUE}
      lead_sites = rbind(lead_sites, 
                         data.frame(site=as.vector(site_params$handle), 
                                    siteid=as.numeric(site_params$dataset.id), 
                                    max_age=max(chron$age),
                                    presett=presett))
      next
    }
    
    if (any(chron$cc==1)){
      chron_uncal = chron[chron$cc==1,]
      calibrated  = Bchroncal(ages=chron_uncal$age, ageSds = chron_uncal$error)
      #  calibrated contains the full calibration curve for each date, we want the weighted mean:
      chron_cal = chron_uncal
      chron_cal$age <- sapply(calibrated, wmean_date)
      accs_new = get_type_accs(chron_cal, site_params$handle)
      accs = rbind(accs, accs_new)
    }
    
    if (any(chron$cc==0)){
      chron_cal = chron[chron$cc==0,]
      accs_new = get_type_accs(chron_cal, site_params$handle)
      accs = rbind(accs, accs_new)
    }
    
  } 
}

# remove negative rates (reversals)
accs = accs[accs$rate>0,]

# remove really large outliers
accs = accs[accs$rate<500,]

accs$era = rep(NA, nrow(accs))
accs$era[accs$age<100] = "modern"
accs$era[accs$age>=100] = "historical"

accs$age_sqrt = sqrt(abs(accs$age))*sign(accs$age)
accs$rate_sqrt = sqrt(accs$rate)

p1 <- ggplot(data=accs) + geom_point(aes(x=age_sqrt, y=rate_sqrt, colour=factor(era)), alpha=0.4) + 
  geom_boxplot(aes(x=age_sqrt, y=rate_sqrt, group=factor(era)), stat='boxplot', alpha=0.4) + geom_vline(xintercept=10, linetype=2)+
  scale_colour_manual(values=c('black', 'grey11'), labels=c('historical', 'modern'), name='Era') +
  scale_fill_manual(values=c('black', 'grey11'), labels=c('historical', 'modern'), name='Era') +
  xlab('Age (years)') + ylab('Rate (yr/cm)') + theme_bw()
print(p1)

p1 <- ggplot(data=accs) + geom_point(aes(x=age_sqrt, y=rate_sqrt, colour=factor(era)), alpha=0.4) + 
  geom_boxplot(aes(x=age_sqrt, y=rate_sqrt, group=factor(era)), stat='boxplot', alpha=0.4) + geom_vline(xintercept=10, linetype=2)+
  scale_colour_manual(values=c('blue', 'black'), labels=c('historical', 'modern'), name='Era') +
  scale_fill_manual(values=c('blue', 'black'), labels=c('historical', 'modern'), name='Era') +
  xlab('Age (years)') + ylab('Rate (yr/cm)') + theme_bw()
print(p1)


ggplotcore_tops = as.data.frame(core_tops)

summary(accs$rate[(accs$age<100) & (accs$rate>0)])
var(accs$rate[(accs$age<100) & (accs$rate>0)], na.rm=TRUE)

summary(accs$rate[(accs$age>=100) & (accs$rate>0) & (accs$age<=2000)])
var(accs$rate[(accs$age>=100) & (accs$rate>0) & (accs$age<=2000)], na.rm=TRUE)

summary(accs$rate[accs$rate>0])

pdf(file='figures/accumulation_rates_vs_age.pdf', width=8, height=6)
plot(sqrt(accs$age), sqrt(accs$rate), xlab='Age (YBP)', ylab='Rate (yr/cm)')
points(sqrt(accs$age[accs$age<100]), sqrt(accs$rate[accs$age<100]), col='blue', pch=19)
legend('topright', legend=c('Modern', 'Historical'), pch=c(19, 1), col=c('blue', 'black'))
dev.off()

pdf(file='figures/accumulation_rates_vs_depth.pdf', width=8, height=6)
plot(accs$depth, accs$rate, xlab='Depth (cm)', ylab='Rate (yr/cm)')
points(accs$depth[accs$age<100], accs$rate[accs$age<100], col='blue', pch=19)
legend('topright', legend=c('Modern', 'Historical'), pch=c(19, 1), col=c('blue', 'black'))
dev.off()

# modern rate gamma parameters
accs.mod = accs[accs$age<100,]
hist(accs.mod$rate)

dat = accs.mod$rate[!is.na(accs.mod$rate)]
dat = dat[dat>0]
gdist <- fitdistr(dat, "gamma")
gdist

x=seq(0,30,by=0.001)
y=dgamma(x, shape=gdist[1]$estimate['shape'], rate=gdist[1]$estimate['rate'])
hist(dat, prob=TRUE)
points(x,y)

# older rate gamma parameters
accs.old = accs[(accs$age>=100) & (accs$age<=2000),]
hist(accs.old$rate)
hist(accs.old$rate[accs.old$rate>0])

dat = accs.old$rate[!is.na(accs.old$rate)]
dat = dat[dat>0]
gdist <- fitdistr(dat, "gamma")
gdist

x=seq(0,200,by=0.001)
y=dgamma(x, shape=gdist[1]$estimate['shape'], rate=gdist[1]$estimate['rate'])
hist(dat, prob=TRUE)
points(x,y)

# everything rate gamma parameters
hist(accs$rate)
hist(accs$rate[accs$rate>0])

dat = accs$rate[!is.na(accs$rate)]
dat = dat[dat>0]
gdist <- fitdistr(dat, "gamma")
gdist

x=seq(0,200,by=0.001)
y=dgamma(x, shape=gdist[1]$estimate['shape'], rate=gdist[1]$estimate['rate'])
hist(dat, prob=TRUE)
points(x,y)

# Modern
# -----
# Rate: 1.06
# Shape: 3.24
pdf(file='figures/modern_accumulation_priors.pdf', width=8, height=6)
x=seq(0,20,by=0.1)
plot(x, dgamma(x, shape=1.5, rate=0.75), type='l', col='black', xlab='Accumulation rate (yr/cm)', ylab='Density', main='Modern')
lines(x, dgamma(x, shape=3.24, rate=1.06), type='l', col='blue')
lines(x, dgamma(x, shape=1.60, rate=0.53), type='l', col='red') # double empirical var and keep mean as empirical mean
legend('topright', legend=c('Data', 'Old', 'Prior'), lty=c(1, 1, 1), col=c('blue', 'black', 'red'))
dev.off()

# 
# Historical
# -----
# Rate: 0.14
# Shape: 2.07
pdf(file='figures/historical_accumulation_priors.pdf', width=8, height=6)
x=seq(0,150,by=1)
plot(x, dgamma(x, shape=1.5, rate=0.15), type='l', col='black', xlab='Accumulation rate (yr/cm)', ylab='Density', main='Historical')
lines(x, dgamma(x, shape=2.07, rate=0.14), type='l', col='blue')
lines(x, dgamma(x, shape=1.05, rate=0.07), type='l', col='red')
legend('topright', legend=c('Data', 'Old', 'Prior'), lty=c(1, 1, 1), col=c('blue', 'black', 'red'))
dev.off()


# write.table(acc.table, file='post_settlement_acc_rates.csv', sep=',', col.names=TRUE, row.names=FALSE)
