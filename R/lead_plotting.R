lead_plots <- function(geochron_tables, all_downloads) {

  binford <- data.frame(age = -60 + c(10, 100, 150),
                        nf_mn  = c(1, 10, 80) / 2,
                        nf_mx  = c(2, 20, 90) / 2)

  ds_name <- all_downloads %>%
    map(function(x) {
      ds <- x$dataset$dataset.meta
      st <- x$dataset$site.data

      data.frame(dataset.id = ds$dataset.id,
                 site.name = st$site.name,
                 stringsAsFactors = FALSE)
    }) %>% bind_rows

  leads <- geochron_tables %>%
    dplyr::filter(!is.na(age) & geo.chron.type %in% "Lead-210") %>%
    dplyr::left_join(ds_name) %>%
    dplyr::select(dataset.id, site.name, age.type,
                  age, e.older, e.younger, geo.chron.type)

  bad_cal_ages <- leads %>%
    filter(!age.type == "Calendar years AD/BC" & age > 800)

  bad_BC_ages <- leads %>%
    filter(age.type == "Calendar years AD/BC" & age < 800)

  assertthat::assert_that(nrow(bad_cal_ages) == 0,
    msg = "There are 210Pb ages returned by Neotoma with calibrated radiocarbon ages in excess of 800 years.")

#  assertthat::assert_that(nrow(bad_BC_ages) == 0,
#    msg = "There are 210Pb ages returned by Neotoma with calibratedradiocarbon ages in excess of 800 years.")

  # We want all ages to be in CE:
  leads <- leads  %>%
      mutate(age.type = ifelse((age.type == "Calendar years AD/BC" & age < 800) |
                                 age.type == "Calendar years BP",
                               "Calendar years BP", "Calendar years AD/BC"))

  leads$age[leads$age.type == "Calendar years AD/BC"] <- 1950 - leads$age[leads$age.type == "Calendar years AD/BC"]

  # None of the ages reported as Calendar years BP are wrong.

  filter_data <- leads %>% filter(e.older > 0)

  lead_smooth <- ggplot() +
    geom_rect(data = binford, aes(xmin = age - 5, xmax = age + 5,
                                  ymin = nf_mn, ymax = nf_mx), fill = 'red', alpha = 0.2) +
    geom_rect(data = binford, aes(xmin = age - 5, xmax = age + 5,
                                  ymin = nf_mn, ymax = nf_mx), fill = NA, color = 'black') +
    geom_point(data = filter_data, aes(x = age, y = e.older), alpha = 0.3) +
    geom_smooth(data = filter_data, aes(x = age, y = e.older), method = 'glm', method.args = list(family = 'poisson')) +
    coord_cartesian(xlim = c(-70, 200), ylim = c(0, 150), expand = c(0, 0)) +
    scale_y_sqrt() +
    xlab("Years Before Present") +
    ylab("Error - SD") +
    theme_bw() +
    theme(axis.title.x = element_text(family = 'serif',
                                      face = 'bold.italic',
                                      size = 18),
          legend.position = 'none',
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

  return(lead_smooth)

}
