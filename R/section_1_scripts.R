
source('R/figures/compare_recalibrations.R')

if (paste0('comp_recal_v', version, '.rds') %in% list.files('data/output')) {
  comp_recal <- readRDS(paste0('data/output/comp_recal_v', version, '.rds'))
} else {
  comp_recal <- compare_recalibrations()
  saveRDS(comp_recal, file = paste0('data/output/comp_recal_v', version, '.rds'))
}

source('R/recalibrate.actual.R', echo = FALSE)

age_seq <- seq(1000, 21000, by = 1000)

new.chrons <- new.chrons %>%  
  filter(!core %in% c("Chalko Lake", "Aghnaghak", "Pittsburg Basin", 
                      "Lac à l'Ange", "Marl Pond", "Cheyenne Bottoms", 
                      "San Agustin Plains")) %>% 
  group_by(handle) %>% 
  mutate(max = findInterval(max(age.lin, na.rm = TRUE), age_seq)) %>% 
  na.omit

basic <- ggplot(new.chrons, aes(x = age.direct, y = age.lin)) + 
  geom_path(aes(group = handle, color = factor(max))) +
  scale_x_continuous(limits = c(-50, 21000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-50, 22000), expand = c(0, 0)) +
  theme_bw() +
  xlab('Direct Age Recalibration') +
  ylab('Linear Model Recalibration') +
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

# Build an age model with a knot every 500 years:
diff.model <- gam(diff ~ s(age.lin, k = 42), 
                  data = new.chrons)

diff.pred <- predict(diff.model,
                     newdata = data.frame(age.lin = seq(0, 21000, by = 10)),
                     se.fit = TRUE, type = 'response')

diff.data <- data.frame(age = seq(0, 21000, by = 10),
                        pred = diff.pred[[1]], 
                        se   = diff.pred[[2]])

diff <- ggplot() +
  geom_point(data = new.chrons, 
             aes(x = age.lin, y = diff, color = as.factor(max))) +
  geom_ribbon(data = diff.data, 
              aes(x = age, ymax = pred + se * 1.96, 
                  ymin = pred - se * 1.96), 
              color = 'red', alpha = 0.5) +
  scale_x_continuous(limits = c(0, 21000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2000, 2000), expand = c(0, 0)) +
  geom_hline(aes(yintercept = 0)) +
  xlab('Linear Model with Recalibration') +
  ylab('Model Difference') +
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

ggsave(file = 'figures/difference.svg', plot = diff, width = 9, height = 2.5)
