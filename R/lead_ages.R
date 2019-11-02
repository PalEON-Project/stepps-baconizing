compare_lead <- function(coredir, geochron_tables, settings, param_file) {

  site_table <- suppressMessages(readr::read_csv(param_file))

  ds_name <- all_downloads %>%
    map(function(x) {
      ds <- x$dataset$dataset.meta
      st <- x$dataset$site.data

      data.frame(dataset.id = ds$dataset.id,
                 site.name = st$site.name,
                 stringsAsFactors = FALSE)
    }) %>%
    dplyr::bind_rows()

  leads <- geochron_tables %>%
    dplyr::filter(!is.na(age) & geo.chron.type %in% "Lead-210") %>%
    dplyr::left_join(ds_name, by = "dataset.id") %>%
    dplyr::select(dataset.id, site.name, age.type, depth,
                  age, e.older, e.younger, geo.chron.type) %>%
    mutate(age.type = ifelse((age.type == "Calendar years AD/BC" & age < 800) |
                                 age.type == "Calendar years BP",
                               "Calendar years BP", "Calendar years AD/BC")) %>%
    left_join(site_table, by = c("dataset.id" = "datasetid"))

  leads$age[leads$age.type == "Calendar years AD/BC"] <- 1950 - leads$age[leads$age.type == "Calendar years AD/BC"]

  bcad <- "Calendar years AD/BC"

  pull_age <- function(x) {

    handle <- x$handle
    depth  <- x$depth

    # Get the age estimate from the Bacon model:
    if (is.na(handle) | is.na(depth) |
        !file.exists(paste0(coredir, '/Cores/', handle, '/', handle, '.csv'))) {
      return(data.frame(bacon_age = as.numeric(NA),
                        bacon_error = as.numeric(NA)))
    }

    age_files <- list.files(paste0(coredir, '/Cores/', handle, '/'),
                            pattern = ".*_ages.txt", full.names = TRUE)

    if (length(age_files) == 0) {
      return(data.frame(bacon_age = as.numeric(NA),
                        bacon_error = as.numeric(NA)))
    }

    if (length(age_files) > 1) {
      cores <- suppressMessages(readr::read_delim(age_files[which.max(file.info(age_files)$ctime)], delim = "\t"))
    } else {
      cores <- suppressMessages(readr::read_delim(age_files, delim = "\t"))
    }

    out <- data.frame(bacon_age   = 1950 - approx(x = cores$depth, cores$mean, xout = depth)$y,
                      bacon_error = approx(x = cores$depth, (cores$mean - cores$min), xout = depth)$y)

    return(out)

  }

  bacon_bound <- leads %>%
    purrrlyr::by_row(function(x) pull_age(x), .collate = 'cols') %>%
    filter(!is.na(bacon_age1))

  # Output plot (element one in the resulting list)

  lead_plot <- ggplot(bacon_bound) +
    geom_abline(slope = -1, alpha = 0.4) +
    geom_errorbar(aes(x = 1950 - age,
                      ymin = bacon_age1 - bacon_error1,
                      ymax = bacon_age1 + bacon_error1), alpha = 0.7) +
    geom_errorbarh(aes(y = bacon_age1,
                       xmax = 1950 - age + e.younger,
                       xmin = 1950 - age - e.older,
                       x = 1950 - age), alpha = 0.7) +
    geom_point(aes(x = 1950 - age, y = bacon_age1), alpha = 0.9) +
    coord_equal(xlim = c(1800, 2000), ylim = c(1800, 2000), expand = c(0,0)) +
    scale_x_reverse() +
    xlab('Reported 210Pb Age') +
    ylab('Bacon Estimated Age') +
    theme_bw() +
    theme(axis.title.x = element_text(family = 'serif',
                                      face = 'bold.italic',
                                      size = 18),
          axis.title.y = element_text(family = 'serif',
                                      face = 'bold.italic',
                                      size = 18),
          axis.ticks = element_blank(),
          axis.text.x = element_text(family = 'serif',
                                     face = 'italic',
                                     size = 14),
          axis.text.y = element_text(family = 'serif',
                                     face = 'italic',
                                     size = 14))


  return(list(plot = lead_plot, data = bacon_bound))

}
