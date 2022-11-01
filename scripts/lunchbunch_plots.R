
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)

foliage_dis <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_dissimilarity')])

ground_dis <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_dissimilarity')])

ul_theme1 <- theme(
  axis.title = element_text(size = 10),
  panel.border = element_rect(fill = NA, color = 'darkslategray', size = 1.2),
  panel.grid = element_line(color = 'cornsilk3'),
  panel.background = element_rect(fill = 'snow1'),
  plot.margin = unit(c(0.05,0.1,0.1,0.1), unit = 'npc'),
  axis.title.x = element_text(vjust = 1))

# visualizing foliage arthropod stats -------------------------------------

foliage_plot_euclidean_canopy <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = canopyCover,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Proportion Canopy Cover',
    y = NULL) +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme1

foliage_plot_euclidean_road <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = distanceToRoad,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = 'Difference in Distance to Nearest Road (km)',
    y = NULL) +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme1

foliage_plot_euclidean_forest <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = forest_1km,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Proportion Forest Cover in a 1km Radius',
    y = NULL) +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme1

foliage_plot_euclidean_trees <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = treeDissimilarity,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Jaccard Dissimilarity of Sampled Tree Species',
    y = NULL) +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme1

foliage_plot_euclidean_distance <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = geographicDistance,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = 'Geographic Distance (km)',
    y = 'Euclidean Distance of Community Composition') +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme1 +
  theme(plot.margin = unit(c(0.05,0.1,0.05,0.1), unit = 'npc'))

foliage_plot_jaccard_canopy <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = canopyCover,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Proportion Canopy Cover',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

foliage_plot_jaccard_road <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = distanceToRoad,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = 'Difference in Distance to Nearest Road (km)',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

foliage_plot_jaccard_forest <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = forest_1km,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Proportion Forest Cover in a 1km Radius',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

foliage_plot_jaccard_trees <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = treeDissimilarity,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Jaccard Dissimilarity of Sampled Tree Species',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

foliage_plot_jaccard_distance <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = geographicDistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'goldenrod') +
  labs(
    x = 'Geographic Distance (km)',
    y = 'Jaccard Dissimilarity of Community Composition') +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1 +
  theme(plot.margin = unit(c(0.05,0.1,0.05,0.1), unit = 'npc'))

foliage_euclidean_env_plots <- ggarrange(
  foliage_plot_euclidean_canopy, 
  foliage_plot_euclidean_road, 
  foliage_plot_euclidean_forest,
  foliage_plot_euclidean_trees,
  ncol = 1) %>% 
  annotate_figure(
    left = text_grob(
      'Euclidean Distance of Community Composition',
      rot = 90,
      vjust = 4,
      size = 10))

foliage_jaccard_env_plots <- ggarrange(
  foliage_plot_jaccard_canopy, 
  foliage_plot_jaccard_road, 
  foliage_plot_jaccard_forest,
  foliage_plot_jaccard_trees,
  ncol = 1) %>% 
  annotate_figure(
    left = text_grob(
      'Jaccard Dissimilarity of Community Composition', 
      rot = 90,
      vjust = 4,
      size = 10))

foliage_env_plots <- ggarrange(
  foliage_euclidean_env_plots,
  foliage_jaccard_env_plots,
  ncol = 2)

ggsave(
  filename = 'figures/lunch_bunch/foliage_env_plots.png',
  plot = foliage_env_plots,
  width = 10,
  height = 8,
  units = 'in')

foliage_dist_plots <- ggarrange(
  foliage_plot_euclidean_distance,
  foliage_plot_jaccard_distance,
  ncol = 2)

ggsave(
  filename = 'figures/lunch_bunch/foliage_distance_plots.png',
  plot = foliage_dist_plots,
  width = 8,
  height = 3.75,
  units = 'in')


# visualizing ground arthropod stats -------------------------------------

ground_plot_euclidean_herb <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = herbaceousCover,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = 'Difference in Herbaceous Cover Class',
    y = NULL) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme1

ground_plot_euclidean_road <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = distanceToRoad,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Distance to Nearest Road (km)',
    y = NULL) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme1

ground_plot_euclidean_litter <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = litterDepth,
    y = euclideanDistance)) +
  geom_point() + 
  labs(
    x = 'Difference in Litter Depth (mm)',
    y = NULL) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme1

ground_plot_euclidean_forest <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = forest_1km,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Proportion Forest Cover in a 1km Radius',
    y = NULL) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme1

ground_plot_euclidean_distance <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = geographicDistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'goldenrod') +
  labs(
    x = 'Geographic Distance (km)',
    y = 'Euclidean Distance of Community Composition') +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme1

ground_plot_jaccard_herb <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = herbaceousCover,
    y = jaccardDissimilarity)) +
  geom_point() + 
  labs(
    x = 'Difference in Herbaceous Cover Class',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

ground_plot_jaccard_road <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = distanceToRoad,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = 'Difference in Distance to Nearest Road (km)',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

ground_plot_jaccard_litter <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = litterDepth,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = 'Difference in Litter Depth (mm)',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

ground_plot_jaccard_forest <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = forest_1km,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'forestgreen') +
  labs(
    x = 'Difference in Proportion Forest cover in a 1km Radius',
    y = NULL) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

ground_plot_jaccard_distance <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = geographicDistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = 'goldenrod') +
  labs(
    x = 'Geographic Distance (km)',
    y = 'Jaccard Dissimilarity of Community Composition') +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme1

ground_euclidean_env_plots <- ggarrange(
  ground_plot_euclidean_herb, 
  ground_plot_euclidean_road, 
  ground_plot_euclidean_litter,
  ground_plot_euclidean_forest,
  ncol = 1) %>% 
  annotate_figure(
    left = text_grob(
      'Euclidean Distance of Community Composition',
      rot = 90,
      vjust = 4,
      size = 10))

ground_jaccard_env_plots <- ggarrange(
  ground_plot_jaccard_herb, 
  ground_plot_jaccard_road, 
  ground_plot_jaccard_litter,
  ground_plot_jaccard_forest,
  ncol = 1) %>% 
  annotate_figure(
    left = text_grob(
      'Jaccard Dissimilarity of Community Composition', 
      rot = 90,
      vjust = 4,
      size = 10))

ground_env_plots <- ggarrange(
  ground_euclidean_env_plots,
  ground_jaccard_env_plots,
  ncol = 2)

ggsave(
  filename = 'figures/lunch_bunch/ground_env_plots.png',
  plot = ground_env_plots,
  width = 10,
  height = 8,
  units = 'in')

ground_dist_plots <- ggarrange(
  ground_plot_euclidean_distance,
  ground_plot_jaccard_distance,
  ncol = 2)

ggsave(
  filename = 'figures/lunch_bunch/ground_dist_plots.png',
  plot = ground_dist_plots,
  width = 8,
  height = 3.75,
  units = 'in')
