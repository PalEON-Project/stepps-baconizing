build_newchrons <- function(all_downloads, chronologies, settings) {

  source("R/recalibrate_actual.R")

  newchron_file <- paste0("data/output/newchrons_", settings$version, ".rds")

  if (!file.exists(newchron_file)) {
    # Build the chronologies.
    # This then saves to a file so we don't have to re-run it every time.

    new_chrons <- lapply(1:length(chronologies), function(x){
      test <- try(rebuild(all_downloads, chronologies, x, settings))

      if ("try-error" %in% class(test)) {
        test <- data.frame(core       = NA,
                           handle     = NA,
                           age.direct = NA,
                           age.lin    = NA,
                           stringsAsFactors = FALSE)
      } else {
        return(test)
      }
    }) %>%
      bind_rows

    saveRDS(new_chrons, file = newchron_file)

  } else {
    new_chrons <- readRDS(newchron_file)
  }

  return(new_chrons)
}
