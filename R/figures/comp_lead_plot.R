comp_lead_plot <- function(comp_doc){

  lead_out <- comp_doc[[2]]

  drop_outlier <- lead_out %>%
    filter(bacon_error1 < 200) %>%
    select(site.name, age, bacon_error1, e.older) %>%
    na.omit() %>%
    arrange(site.name, age)

  lead_comp_plot <- ggplot(data = drop_outlier) +
    geom_point(aes(x = age, y = bacon_error1, shape = site.name),
               alpha = 0.7, size = 3) +
    geom_point(aes(x = age, y = e.older, shape = site.name),
               color = "red", alpha = 0.7, size = 3) +
    geom_ribbon(aes(x = age, ymin = e.older,
                    ymax= bacon_error1, group = site.name),
                alpha = 0.1, color = alpha(colour = "black", 0.2)) +
    geom_segment(aes(x = age, xend = age,
                     y = bacon_error1, yend = e.older), alpha = 0.3) +
    scale_x_continuous(limits=c(-50, 100)) +
    scale_y_continuous(limits=c(0, 80)) +
    coord_equal(expand = c(0,0)) +
    xlab("Years Before Present") +
    ylab("Model Uncertainty") +
    annotate("text", x = -25, y = 80,
      label = "Bacon Uncertainty") +
    annotate("text", x = -25, y = 75,
      label = "210Pb Uncertainty", color = "red") +
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

  ggplot2::ggsave(filename = "figures/lead_comp_plot.svg",
    plot = lead_comp_plot,
    width = 8, height = 5)
}
