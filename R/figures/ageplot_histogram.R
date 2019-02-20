
chronology_type_histogram <- function(all_downloads, pubs, settings) {

  dsid <- sapply(all_downloads,
    function(x) {
      x$dataset$dataset.meta$dataset.id
    })

  age_priority <- c("Calendar years BP",
                    "Varve years BP",
                    "Calibrated radiocarbon years BP",
                    "Radiocarbon years BP")

  chron <- sapply(all_downloads,
    function(x){
      # Doing this because it appears the the default age type
      # is not always respected.

      types <- sapply(x$chronologies, function(x) x$age.type[1])
      age_types <- types[which.min(match(types, age_priority))]

      if (length(age_types) == 0) {
        age_types <- NA
      }

      return(age_types)
  })

  meta_table <- pubs %>%
    map(function(x){
      metas <- do.call(rbind.data.frame,
                       lapply(x, "[[", "meta"))
      output <- metas[which.min(metas$year), ]
      if (nrow(output) == 0) {
        output <- data.frame(id = NA,
                             pub.type = NA,
                             year = NA,
                             citation = NA)
      }

      return(output)
    }) %>%
    bind_rows %>%
    mutate(chron = chron,
            dsid = dsid) %>%
    group_by(dsid) %>%
    filter(year == max(year) & !is.na(chron)) %>%
    ungroup() %>%
    mutate(chron = factor(chron, levels = age_priority)) %>%
    unique() %>%
    na.omit()

  plot_out <- ggplot(na.omit(meta_table), aes(x = year, fill = chron)) +
    geom_histogram(binwidth = 2, color = "black") +
    coord_cartesian(xlim = c(1948, 2014), expand = FALSE) +
    scale_x_continuous(breaks = seq(1950, 2010, by = 10)) +
    scale_fill_viridis(guide = guide_legend(title = "Chronology"),
                      discrete = TRUE) +
    xlab("Publication Year") +
    ylab("Core Count") +
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

  ggplot2::ggsave(filename = "figures/histograms.svg",
                  plot = plot_out,
                  width = 10,
                  height = 4)

  meta_out <- paste0("data/output/chronologytables_",
                     settings$version, ".csv")

  readr::write_csv(meta_table, meta_out)

  return(plot_out)

}
