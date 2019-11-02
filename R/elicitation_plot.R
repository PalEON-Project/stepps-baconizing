## Expert Elicitation

expert_elicitation <- function() {

# To be modified, maybe this works better:
# ggplot(plot_table, aes(x = original, y = diff, color = class)) + geom_point() + coord_cartesian(xlim=c(1700,2000), ylim = c(-200, 150))



    all_sites <- readr::read_csv("data/input/bacon_params_v1.csv")
  elicitation <- readr::read_csv("data/input/expert_assessment.csv")
    pollen_ts <- readr::read_csv("data/sediment_ages_v1.0_varves.csv")

  core_estimates <- function(x) {
    x <- x %>%
      mutate(depth_inc = sort(c(pre1, pre2, pre3, pre4))[2]) %>%
      dplyr::select(id = datasetID,
                    handle    = handle,
                    lat,
                    long,
                    depth_inc)
  }

  elicit <- by_row(elicitation, core_estimates)$`.out` %>%
   bind_rows %>%
   filter(depth_inc > 0)

  elicit$sett_age <- raster::raster('data/input/age_of_sample.tif') %>%
    raster::projectRaster(., crs="+init=epsg:4326") %>%
    raster::extract(y = elicit[,c("long", "lat")])

  # Now I need to get the original ages, sett age & Bacon ages:
  # We define the hiatus as the settlement year in the Bulk Baconizing document:

  site_names <- all_downloads %>%
    map_chr(function(x) x$dataset$site$site.name)

  handles <- sapply(all_downloads,
    function(x) {
      x$dataset$dataset.meta$collection.handle
    })

  test_ages <- function(x, ds_handles = handles) {

   if(x$depth_inc > 1 &
      x$id %in% pollen_ts$id &
      x$handle %in% ds_handles) {

      good_handle <- match(x$handle,
                           ds_handles)

      orig   <- all_downloads[[good_handle]]$sample.meta$age[x$depth_inc]
      atype  <- all_downloads[[good_handle]]$sample.meta$age.type[1]

      output <- pollen_ts %>%
        filter(id %in% x$id) %>%
        filter(row_number() %in% x$depth_inc) %>%
        dplyr::select(dplyr::contains("bacon")) %>%
        unlist %>%
        mean(na.rm = TRUE)

    output <- data.frame(bacon = 1950 - output,
                         original = 1950 - orig,
                         type = atype,
                         stringsAsFactors = FALSE)

   } else {
     output <- data.frame(bacon = NA,
                          original = NA,
                          type = NA_character_,
                          stringsAsFactors = FALSE)
   }
  }

  recal <- purrrlyr::by_row(elicit, test_ages)$`.out` %>%
    bind_rows()

  plot_table <- data.frame(  original = c(recal$original, recal$bacon),
                          settlement = c(elicit$sett_age, elicit$sett_age),
                               class = c(rep(c("Original Age Model",
                                               "Bacon Age Model"),
                                               each = nrow(elicit))),
                                             stringsAsFactors = FALSE)

  plot_table$class <- factor(plot_table$class, levels = c("Original Age Model",
                                                         "Bacon Age Model"))

  output_plot <- ggplot(plot_table %>% filter(original > 1000 & settlement > 1000),
                       aes(x = settlement, y = original)) +
   geom_point() +
   facet_wrap(~class) +
   coord_cartesian(xlim = c(1820, 1920),
                   ylim = c(1200, 2000),
                   expand = FALSE) +
   geom_abline(slope = 1, intercept = 0) +
   geom_smooth(method = "lm") +
   scale_x_reverse() +
   xlab("Assigned Settlement Age Control (CE)") +
   ylab("Modeled Age (CE)") +
   theme_bw() +
   theme(axis.title.x = element_text(family = "serif",
                                     face = "bold.italic",
                                     size = 18),
         legend.position = "none",
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

  orig_bacon <- ggplot(recal %>% filter(bacon > 100 & original > 1000),
                      aes(x = original, y = bacon)) +
   geom_point() +
   geom_abline(slope = 1, intercept = 0) +
   geom_smooth(method = "lm") +
   xlab("Original Age Model") +
   ylab("Bacon Age Model") +
   theme_bw() +
   theme(axis.title.x = element_text(family = "serif",
                                     face = "bold.italic",
                                     size = 18),
         legend.position = "none",
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

  return(list(output_plot, orig_bacon))

}
