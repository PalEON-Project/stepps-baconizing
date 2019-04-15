###################################################################################################################################################
## Determine accumulation rate prior for the upper midwestern US using
## chronological controls from Neotoma
###################################################################################################################################################
library(MASS)
library(ggplot2)

# load Bchron
source("R/utils/calibrate.R")

# meta data; includes list of sites
bacon_params <- read.csv("data/input/bacon_params_v1.csv",
                         header = TRUE,
                         sep = ",",
                         stringsAsFactors = FALSE)

ncores <- nrow(bacon_params)

get_type_accs <- function(chron, handle){

  rate  <-  diff(chron$age) / diff(chron$depth)
  depth <- chron$depth[1:(nrow(chron) - 1)] + diff(chron$depth) / 2
  thick <- diff(chron$depth)
  age   <- chron$age[1:(nrow(chron) - 1)] + diff(chron$age) / 2
  site  <- rep(handle, nrow(chron) - 1)
  cc    <- rep(unique(chron$cc), nrow(chron) - 1)

  return(data.frame(site = site,
                    depth = depth,
                    thick = thick,
                    age = age,
                    rate = rate,
                    cc = cc))
}

wmean_date <- function(x) sum(x$ageGrid * x$densities / sum(x$densities))

get_accs <- function(bacon_params){

  accs     <- data.frame(site = character(0),
                         depth = numeric(0),
                         thick = numeric(0),
                         age = numeric(0),
                         rate = numeric(0),
                         cc = numeric(0))
  core_tops <- list()

  lead_sites <- data.frame(site = character(0),
                           siteid = numeric(0),
                           max_age = numeric(0),
                           presett = numeric())

  for (i in 1:ncores) {

    site_params <- bacon_params[i, ]
    if (site_params$suit){

      if (!(file.exists(sprintf("Cores/%s/%s.csv", site_params$handle, site_params$handle)))) {
        print(paste0("No file exists for site ", site_params$handle))
        next
      }

      chron = read.table(sprintf("Cores/%s/%s.csv", site_params$handle, site_params$handle), sep=",", header=TRUE)
      if ( any(substr(chron$labid, 1, 4) == "Core") ){
        # print(chron[which(substr(chron$labid, 1, 4) == "Core"),])
        core_tops = rbind(core_tops, data.frame(site=site_params$handle, chron[which(substr(chron$labid, 1, 4) == "Core"),]))
      }
      if (any(substr(chron$labid,1,4) == "Lead")){
        presett=FALSE
        if  (any(substr(chron$labid,1,3) == "Pre")){presett=TRUE}
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
  accs = accs[!is.na(accs$rate),]

  # remove negative rates (reversals)
  accs = accs[accs$rate>0,]

  # remove really large outliers
  accs = accs[accs$rate<500,]

  accs$era = rep(NA, nrow(accs))
  accs$era[accs$age<100] = "modern"
  accs$era[accs$age>=100] = "historical"

  accs$age_sqrt = abs(accs$age)*sign(accs$age)
  accs$rate_sqrt = accs$rate

  return(accs)
}

library(gridExtra)

plot_acc_rates <- function(accs){

  p1 <- ggplot(data = accs) +
    geom_point(aes(x = age_sqrt, y = rate_sqrt, colour = factor(era)),
               alpha = 0.4) +
    geom_boxplot(aes(x = age_sqrt, y = rate_sqrt, group = factor(era)),
                 stat = "boxplot", alpha = 0.4,
                 outlier.color = NA, varwidth = TRUE, coef = 2) +
    coord_cartesian(expand = FALSE) +
    scale_x_sqrt() +
    scale_y_sqrt() +
    geom_vline(xintercept=100, linetype=2) + # had to square this value!
    scale_colour_manual(values=c("black", "red"), labels=c("historical", "modern"), name="Era") +
    scale_fill_manual(values=c("black", "red"), labels=c("historical", "modern"), name="Era") +
    xlab("Interval Midpoint Age \n(calibrated Radiocarbon Years)") + ylab("Accumulation Rate (yr/cm)") +
    theme_bw() +
    theme(axis.title.x = element_text(family = "serif",
                                      face = "bold.italic",
                                      size = 18),
          axis.title.y = element_text(family = "serif",
                                      face = "bold.italic",
                                      size = 18),
          axis.ticks = element_blank(),
          axis.text.x = element_text(family = "serif",
                                     face = "italic",
                                     size = 14),
          axis.text.y = element_text(family = "serif",
                                     face = "italic",
                                     size = 14))

  p2 <- ggplot(data=accs) +
    geom_density(aes(x=rate_sqrt, fill=factor(era)), alpha=0.4) +
    coord_cartesian(expand = FALSE) +
    scale_x_sqrt() +
    # scale_colour_manual(values=c("blue", "black"), labels=c("historical", "modern"), name="Era") +
    # scale_fill_manual(values=c("blue", "black"), labels=c("historical", "modern"), name="Era") +
    xlab("Rate (year/cm)") + ylab("Count") +
    theme_bw() +
    guides(fill=guide_legend(title="Era")) +
    theme(axis.title.x = element_text(family = "serif",
                                      face = "bold.italic",
                                      size = 18),
          axis.title.y = element_text(family = "serif",
                                      face = "bold.italic",
                                      size = 18),
          axis.ticks = element_blank(),
          axis.text.x = element_text(family = "serif",
                                     face = "italic",
                                     size = 14),
          axis.text.y = element_text(family = "serif",
                                     face = "italic",
                                     size = 14))

  return(grid.arrange(p1, p2))
}

accs <- get_accs(bacon_params)
