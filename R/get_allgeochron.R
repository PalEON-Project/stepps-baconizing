get_allgeochron <- function(settings) {

  null_to_na <- function(x) ifelse(is.null(x), NA, x)

  control_file <- paste0("data/output/all_geochron_", settings$version, ".rds")

  if (file.exists(control_file)) {
    all_geochron <- readRDS(control_file)
  } else {
    all_geochron <- list()

    all_dl <- get_dataset(datasettype = "pollen")

    for (i in 1:length(all_dl)) {
      dsid <- all_dl[[i]]$dataset.meta$dataset.id

      url <- paste0("http://api-dev.neotomadb.org/v2.0/data/datasets/",
        dsid, "/chronology")

      chrons <- jsonlite::fromJSON(url, simplifyVector = FALSE)$data[[1]]

      if ("chronologies" %in% names(chrons)) {
        geochron <- lapply(chrons$chronologies[[1]]$controls,
          function(x) x$geochron)

        depths <- sapply(chrons$chronologies[[1]]$controls,
          function(x) null_to_na(x$depth))

        all_geochron[[i]] <- data.frame(
          dataset.id = dsid,
          depth = depths,
          age.type = sapply(geochron, function(x) null_to_na(x$agetype)),
          age = sapply(geochron, function(x) null_to_na(x$age)),
          e.older = sapply(geochron, function(x) null_to_na(x$erroryounger)),
          e.younger = sapply(geochron, function(x) null_to_na(x$errorolder)),
          geo.chron.type = sapply(geochron,
            function(x) null_to_na(x$geochrontype)),
          stringsAsFactors = FALSE)

      } else {
        all_geochron[[i]] <- data.frame(dataset.id = dsid,
                                        depth = NA,
                                        age.type = NA,
                                        age = NA,
                                        e.older = NA,
                                        e.younger = NA,
                                        geo.chron.type = NA,
                                        stringsAsFactors = FALSE)
      }
    }

    all_geochron <- all_geochron %>%
      bind_rows()

    saveRDS(all_geochron, control_file)

  }

  return(all_geochron)
}
