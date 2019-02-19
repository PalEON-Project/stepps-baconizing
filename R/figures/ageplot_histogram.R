
chronology_type_histogram <- function(meta.table) {
  
  meta.table_short <- meta.table %>% 
    group_by(id) %>% 
    filter(year == max(year) & !is.na(chron)) %>% 
    unique()
  
  # TODO: Because there are defaults for each model type some of these records have multiple
  # entries, for example, both calibrated and uncalibrated ages.  Need to pick the best.
  
  meta.table_short$chron <- factor(meta.table_short$chron,
                                   levels = c("Calendar years BP",
                                              "Varve years BP",
                                              "Calibrated radiocarbon years BP",
                                              "Radiocarbon years BP"))
  
  for (i in meta.table_short$id[duplicated(meta.table_short$id)]) {
    sample <- meta.table_short$id == i
    drop   <- which.max(meta.table_short$chron[sample])
    meta.table_short$chron[sample][drop] <- NA
  }
  
  meta.table_short <- meta.table_short %>% na.omit
  
  plot_out <- ggplot(na.omit(meta.table_short), aes(x = year, fill = chron)) + 
    geom_histogram(binwidth = 2, color = 'black') +
    coord_cartesian(xlim= c(1948, 2014), ylim= c(0, 50), expand = FALSE) +
    scale_x_continuous(breaks = seq(1950, 2010, by = 10)) +
    scale_fill_viridis(guide = guide_legend(title = "Chronology"), 
                      discrete = TRUE) +
    xlab('Publication Year') + 
    ylab('Core Count') +
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
  
  ggsave(filename = 'figures/histograms.svg', plot_out, width = 10, height = 4)
  
  return(plot_out)

}