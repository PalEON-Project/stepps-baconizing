#' @title Rebuild Chronology with BChron
#' @param x A chronology object from Neotoma
#' @param settings The project settings.

rebuild <- function(all_downloads, chronologies, x, settings){
  # This function works to rebuild the age models for cores, it tests first
  # to see if there is a built age model (that's the call to `meta` in the
  # first if statement).

  if ("meta" %in% names(chronologies[[x]])) {

    # If there's a model and it's radiocarbon years,
    # then we directly calibrate it to
    # calibrated radiocarbon years using Bchron.

    ch <- chronologies[[x]]
    dl <- all_downloads[[x]]
    ds <- all_downloads[[x]]$dataset

    if (ch$meta$age.type == "Radiocarbon years BP" &
         nrow(ch$chron.control) > 1) {

      ages <- dl$sample.meta$age
      good.ages <- ages > 71 & ages < 40000 & !is.na(ages)

      if (sum(good.ages) > 0) {
        calibed <- Bchron::BchronCalibrate(round(ages[good.ages], 0),
                                   ageSds    = rep(1, sum(good.ages)),
                                   calCurves = rep("intcal13", sum(good.ages)))

        from.calib <- sapply(calibed,
          function(x)weighted.mean(x$ageGrid, x$densities))

        controls <- ch$chron.control$age

        controls[is.na(controls)] <- ( (ch$chron.control$age.young +
          ch$chron.control$age.old) / 2 +
          ch$chron.control$age.young)[is.na(controls)]

        errors   <- abs(ch$chron.control$age - ch$chron.control$age.young)

        calib <- !ch$chron.control$control.type %in%
          c("Core top", "Annual laminations (varves)",
            "Lead-210", "Radiocarbon, calibrated") &
           controls < 40000 & controls > 71

        errors[is.na(errors)] <- 200

        if (sum(calib) > 0) {
          lin.ages <- Bchron::BchronCalibrate(controls[calib],
                                      ageSds = errors[calib],
                                      calCurves = rep("intcal13", sum(calib)))

          controls[calib] <- sapply(lin.ages,
            function(x)weighted.mean(x$ageGrid, x$densities))

        }

        new.ages <- approx(x    = ch$chron.control$depth,
                           y    = controls,
                           xout = dl$sample.meta$depth)

        if (all(diff(new.ages$y[good.ages]) > 0, na.rm = TRUE)) {
          # Given that this is "rough & ready",
          # eliminate all cores with age reversals.
          output <- data.frame(core       = ds$site$site.name,
                               handle     = ds$dataset.meta$collection.handle,
                               age.direct = from.calib,
                               age.lin    = new.ages$y[good.ages])
        } else {
          output <- data.frame(core       = ds$site.data$site.name,
                               handle     = ds$dataset.meta$collection.handle,
                               age.direct = NA,
                               age.lin    = NA)
        }
      } else {
        # If there's no data then we push out an NA
        # but still keep the site name, for tracking)

        output <- data.frame(core       = ds$site.data$site.name,
                             handle     = ds$dataset.meta$collection.handle,
                             age.direct = NA,
                             age.lin    = NA)
      }
    } else {
      output <- data.frame(core = ds$site$site.name,
                           handle = ds$dataset.meta$collection.handle,
                           age.direct = NA,
                           age.lin = NA)
    }

  } else {
    output <- data.frame(core = ds$site$site.name,
                         handle = ds$dataset.meta$collection.handle,
                         age.direct = NA,
                         age.lin = NA)
  }

  output$diff <- output$age.lin - output$age.direct

  output
}

#  This is just to debug, so I can see where errors occur.
# for (i in 1:length(chronologies)) {
#  rebuild(all_downloads, chronologies, i, settings)
#}
