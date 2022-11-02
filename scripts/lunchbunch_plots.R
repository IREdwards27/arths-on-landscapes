
# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggfortify)
library(janitor)

foliage_dis <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_dissimilarity')])

ground_dis <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_dissimilarity')])

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

beatsheets <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^beatsheets')])

trees <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])

circles <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^circles')])

taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])

pitfalls <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^pitfallsurveys')])

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


# foliage arthropod PCA plot ----------------------------------------------

# this is copied from dissimilarity_calculations, so annotations are removed to save space
foliage_families <- foliage_arths %>%
  mutate(TaxonID = case_when(
    # switch all Trogossitidae to Nitidulidae
    TaxonID == 678393 ~ 114290,
    # switch all Ponera and Hypoponera to Brachyponera chinensis
    TaxonID %in% c(574209,574195) ~ 11,
    TRUE ~ TaxonID)) %>%
  left_join(
    taxa,
    by = 'TaxonID') %>%
  filter(
    order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>%
  left_join(
    beatsheets %>%
      select(BeatSheetID, Date, TreeFK),
    by = c('BeatSheetFK' = 'BeatSheetID')) %>%
  left_join(
    trees %>%
      select(TreeID, CircleFK),
    by = c('TreeFK' = 'TreeID')) %>%
  left_join(
    circles,
    by = c('CircleFK' = 'CircleID')) %>%
  group_by(CircleFK, family) %>%
  summarize(
    n_individuals = sum(Quantity, na.rm = T),
    biomass = sum(TotalMass))

hi_foliage_fams <- foliage_families %>% 
  mutate(
    biomass = if_else(
      is.na(biomass),
      true = 0,
      false = biomass),
    logBiomass = log(biomass + 0.01)) %>% 
  group_by(family) %>% 
  summarize(range = max(logBiomass) - min(logBiomass)) %>% 
  filter(!is.na(family)) %>% 
  arrange(desc(range)) %>%
  filter(range > 8) %>%
  pull(family)

# make a foliage plot that can be PCA'ed - columns are families, rows are circles
foliage_base1 <- map_dfc(
  unique(foliage_families$CircleFK),
  ~ foliage_families %>%
    filter(
      CircleFK == .x,
      !is.na(family),
      family %in% hi_foliage_fams) %>%
    bind_rows(tibble(
      family = hi_foliage_fams[!hi_foliage_fams %in% .$family])) %>%
    mutate(
      CircleFK = if_else(
        is.na(CircleFK),
        true = .x,
        false = CircleFK),
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      logBiomass = log(biomass+0.01)) %>%
    select(!n_individuals:biomass) %>%
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as_tibble(rownames = 'family') %>%
    arrange(family) %>%
    select(!family)) %>%
  cbind(family = hi_foliage_fams) %>%
  relocate(family) %>%
  as_tibble() %>%
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x))) %>% 
  # switch rows and columns
  t() %>% 
  # get column names from the first row
  row_to_names(row_number = 1) %>% 
  as_tibble() %>% 
  mutate(across(.fns = as.numeric)) %>% 
  cbind(CircleID = circles$CircleID) %>% 
  left_join(
    circles %>% 
      select(CircleID, SiteFK),
    by = 'CircleID')

rownames(foliage_base1) <- circles$CircleID

foliage_pca <- prcomp(foliage_base1[1:10], scale = T)

foliage_pca_plot <- autoplot(
  foliage_pca,
  data = foliage_base1,
  colour = 'SiteFK',
  loadings = T,
  loadings.label = T,
  size = 3,
  loadings.colour = 'forestgreen',
  loadings.label.size = 3) +
  ul_theme1 +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.01,0.01,0.01,0.01), unit = 'npc')) +
  scale_color_viridis_d()

ggsave(
  plot = foliage_pca_plot,
  filename = 'figures/lunch_bunch/foliage_pca.png',
  width = 8,
  height = 4,
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

# ground arthropod PCA plot -----------------------------------------------

ground_families <- ground_arths %>%
  # filter to confident IDs while still working on bug ID
  mutate(TaxonID = case_when(
    # switch all Trogossitidae to Nitidulidae
    TaxonID == 678393 ~ 114290,
    # switch all Ponera and Hypoponera to Brachyponera chinensis
    TaxonID %in% c(574209,574195) ~ 11,
    TRUE ~ TaxonID)) %>%
  left_join(
    taxa,
    by = 'TaxonID') %>%
  # filter to focal orders
  filter(
    order %in% c('Araneae','Archaeognatha','Coleoptera','Isopoda', 'Opiliones', 'Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>%
  left_join(
    pitfalls %>%
      select(PitfallID, DateCollected, CircleID),
    by = 'PitfallID') %>%
  left_join(
    circles,
    by = 'CircleID') %>%
  group_by(CircleID, family) %>%
  summarize(
    n_individuals = sum(Number, na.rm = T),
    biomass = sum(TotalMass))

hi_ground_fams <- ground_families %>% 
  mutate(
    biomass = if_else(
      is.na(biomass),
      true = 0,
      false = biomass),
    logBiomass = log(biomass + 0.01)) %>% 
  group_by(family) %>% 
  summarize(range = max(logBiomass) - min(logBiomass)) %>% 
  filter(!is.na(family)) %>% 
  arrange(desc(range)) %>% 
  filter(range > 4) %>%
  pull(family)

# make a foliage plot that can be PCA'ed - columns are families, rows are circles
ground_base1 <- map_dfc(
  unique(ground_families$CircleID),
  ~ ground_families %>%
    filter(
      CircleID == .x,
      !is.na(family),
      family %in% hi_ground_fams) %>%
    bind_rows(tibble(
      family = hi_ground_fams[!hi_ground_fams %in% .$family])) %>%
    mutate(
      CircleID = if_else(
        is.na(CircleID),
        true = .x,
        false = CircleID),
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      logBiomass = log(biomass+0.01)) %>%
    select(!n_individuals:biomass) %>%
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as_tibble(rownames = 'family') %>%
    arrange(family) %>%
    select(!family)) %>%
  cbind(family = hi_ground_fams) %>%
  relocate(family) %>%
  as_tibble() %>%
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x))) %>% 
  # switch rows and columns
  t() %>% 
  # get column names from the first row
  row_to_names(row_number = 1) %>% 
  as_tibble() %>% 
  mutate(across(.fns = as.numeric)) %>% 
  cbind(CircleID = circles$CircleID) %>% 
  left_join(
    circles %>% 
      select(CircleID, SiteFK),
    by = 'CircleID')

rownames(ground_base1) <- circles$CircleID

ground_pca <- prcomp(ground_base1[1:12], scale = T)

ground_pca_plot <- autoplot(
  ground_pca,
  data = ground_base1,
  colour = 'SiteFK',
  loadings = T,
  loadings.label = T,
  size = 3,
  loadings.colour = 'forestgreen',
  loadings.label.size = 3) +
  ul_theme1 +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.01,0.01,0.01,0.01), unit = 'npc')) +
  scale_color_viridis_d()

ggsave(
  plot = ground_pca_plot,
  filename = 'figures/lunch_bunch/ground_pca.png',
  width = 8,
  height = 4,
  units = 'in')

