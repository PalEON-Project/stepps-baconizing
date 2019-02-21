get_allpublications <- function(all_downloads, settings) {

  pub_file <- paste0("data/output/pubs_v", settings$version, ".rds")

  pubs_exist <- file.exists(pub_file)

  if (pubs_exist == TRUE) {
    pubs <- readRDS(paste0("data/output/pubs_v", settings$version, ".rds"))
  } else {
    pubs <- neotoma::get_publication(all_downloads)
    saveRDS(pubs, file = paste0("data/output/pubs_v", settings$version, ".rds"))
  }

  return(pubs)
}
