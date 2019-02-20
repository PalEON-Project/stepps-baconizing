plot_recal_diffs <- function(new_chrons) {

  basic <- ggplot(new_chrons, aes(x = age.direct, y = age.lin)) +
    geom_path(aes(group = handle, color = factor(max))) +
    coord_cartesian(xlim = c(-50, 21000),
                    ylim = c(-50, 22000), expand = FALSE) +
    theme_bw() +
    xlab("Direct Age Recalibration") +
    ylab("Linear Model Recalibration") +
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

  # Build an age model with a knot every 500 years:
  diff.model <- gam(diff ~ s(age.lin, k = 42),
                    data = new_chrons)

  diff.pred <- predict(diff.model,
                       newdata = data.frame(age.lin = seq(0, 21000, by = 10)),
                       se.fit = TRUE, type = "response")

  diff.data <- data.frame(age = seq(0, 21000, by = 10),
                          pred = diff.pred[[1]],
                          se   = diff.pred[[2]])

  diff <- ggplot() +
    geom_point(data = new_chrons,
               aes(x = age.lin, y = diff, color = as.factor(max))) +
    geom_ribbon(data = diff.data,
                aes(x = age, ymax = pred + se * 1.96,
                    ymin = pred - se * 1.96),
                color = "red", alpha = 0.5) +
    coord_cartesian(xlim = c(0, 21000), ylim = c(-2000, 2000), expand = FALSE) +
    geom_hline(aes(yintercept = 0)) +
    xlab("Linear Model with Recalibration") +
    ylab("Model Difference") +
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

  ggplot2::ggsave(file = "figures/difference.svg",
                  plot = diff,
                  width = 9, height = 2.5)
}
