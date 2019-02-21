get_allchronologies <- function(all_downloads, settings) {

  chron_file <- paste0("data/output/chronologies_v",
                       settings$version, ".rds")

  if (!file.exists(chron_file)) {
    chronologies <- list()

    for (i in 1:length(all_downloads)) {
      chronologies[[i]] <- try(get_chroncontrol(all_downloads[[i]]),
                               silent = TRUE)
      flush.console()
      if ("try-error" %in% class(chronologies[[i]])) {
        chronologies[[i]] <- list(empty = NA)
      }
    }

    saveRDS(chronologies,
            file = chron_file)

    } else{
      chronologies <- readRDS(chron_file)
    }

  return(chronologies)

}
