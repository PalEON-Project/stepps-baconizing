get_allcontrols <- function(all_downloads, settings) {

  control_file <- paste0("data/output/controls_", settings$version, ".rds")

  if (file.exists(control_file)) {
    controls <- readRDS(control_file)
  } else {
    controls <- lapply(all_downloads,
      function(x) {
        try(neotoma::get_chroncontrol(x))
      })

    for (i in length(controls):1) {
      if ("try-error" %in% class(controls[[i]])) {
        controls[[i]] <- NULL
      }
    }

    saveRDS(controls, control_file)
  }

  return(controls)
}
