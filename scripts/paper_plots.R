# setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggfortify)
library(janitor)
library(ggrepel)

foliage_dis <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_dissimilarity')])

ground_dis <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_dissimilarity')])

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_arths')])

beatsheets <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^beat_sheets')])

trees <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])

circles <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^circles')])

taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_arths')])

pitfalls <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^pitfalls')])

functions <- read_csv('data/families.csv')

ul_theme2 <- theme(
  axis.title = element_text(size = 10),
  panel.border = element_rect(fill = NA, color = 'darkslategray', size = 1.2),
  panel.grid = element_line(color = 'cornsilk3'),
  panel.background = element_rect(fill = 'snow1'),
  plot.margin = unit(c(0.05,0.1,0.1,0.1), unit = 'npc'),
  axis.title.x = element_text(vjust = 1))

colorz  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# visualizing foliage arthropod stats -------------------------------------

foliage_plot_euclidean_canopy <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = canopyCover,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = "\u0394 Local Canopy Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

foliage_plot_euclidean_road <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = distanceToRoad,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = "\u0394 Distance to Nearest Road (km)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

foliage_plot_euclidean_forest <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = forest_1km,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#009E73') +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 0.5,
    y = 7.5,
    label = 'R2 = 0.11, p < 0.001',
    color = '#009E73') +
  theme(axis.title = element_text(size = 14))

foliage_plot_euclidean_trees <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = treeDissimilarity,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#009E73') +
  labs(
    x = "Tree Species Dissimilarity",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 0.5,
    y = 7.5,
    label = 'R2 = 0.12, p < 0.005',
    color = '#009E73') +
  theme(axis.title = element_text(size = 14))

foliage_plot_euclidean_distance <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = geographicDistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#E69F00') +
  labs(
    x = "Geographic Distance (km)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 25,
    y = 7.5,
    label = 'R2 = 0.07, p < 0.005',
    color = '#E69F00') +
  theme(axis.title = element_text(size = 14))

foliage_plot_jaccard_canopy <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = canopyCover,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = "\u0394 Local Canopy Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

foliage_plot_jaccard_road <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = distanceToRoad,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = "\u0394 Distance to Nearest Road (km)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

foliage_plot_jaccard_forest <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = forest_1km,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#009E73') +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 0.5,
    y = 0.2,
    label = 'R2 = 0.13, p < 0.001',
    color = '#009E73') +
  theme(axis.title = element_text(size = 14))

foliage_plot_jaccard_trees <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = treeDissimilarity,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#009E73') +
  labs(
    x = "Tree Species Dissimilarity",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 0.5,
    y = 0.2,
    label = 'R2 = 0.16, p < 0.0005',
    color = '#009E73') +
  theme(axis.title = element_text(size = 14))

foliage_plot_jaccard_distance <- ggplot(
  data = foliage_dis,
  mapping = aes(
    x = geographicDistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#E69F00') +
  labs(
    x = "Geographic Distance (km)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 25,
    y = 0.2,
    label = 'R2 = 0.09, p < 0.005',
    color = '#E69F00') +
  theme(axis.title = element_text(size = 14))

foliage_euclidean_plots <- ggarrange( 
  foliage_plot_euclidean_forest,
  foliage_plot_euclidean_road, 
  foliage_plot_euclidean_canopy,
  foliage_plot_euclidean_trees,
  foliage_plot_euclidean_distance,
  ncol = 1,
  labels = c('a)','c)','e)','g)','i)'),
  label.x = -0.01)

foliage_jaccard_plots <- ggarrange( 
  foliage_plot_jaccard_forest, 
  foliage_plot_jaccard_road,
  foliage_plot_jaccard_canopy,
  foliage_plot_jaccard_trees,
  foliage_plot_jaccard_distance,
  ncol = 1,
  labels = c('b)','d)','f)','h)','j)'),
  label.x = -0.01)

foliage_plots <- ggarrange(
  foliage_euclidean_plots,
  foliage_jaccard_plots,
  ncol = 2)

ggsave(
  'figures/paper/foliage_plots.png',
  plot = foliage_plots,
  width = 9.75,
  height = 13.5,
  units = 'in')


# foliage arthropod PCA plot ---------------------------------------------

# this is copied from dissimilarity_calculations, so annotations are removed to save space
foliage_families <- foliage_arths %>%
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

all_foliage <- map(
  unique(foliage_families$CircleFK),
  function(x){
    all_families <-  unique(foliage_families$family)
    
    in_families <-  foliage_families %>% 
      filter(CircleFK == x) %>% 
      pull(family) %>% 
      unique()
    
    out_families <- all_families[!all_families %in% in_families]
    
    rbind(
      foliage_families %>% 
        filter(CircleFK == x),
      tibble(
        CircleFK = rep(x, length(out_families)),
        family = out_families,
        n_individuals = rep(0, length(out_families)),
        biomass = rep(0, length(out_families))))
  }) %>% 
  bind_rows() %>% 
  arrange(CircleFK, family)

foliage_functions <- foliage_families %>% 
  ungroup() %>% 
  filter(!is.na(family)) %>% 
  select(family) %>% 
  distinct() %>% 
  left_join(functions, by = 'family') %>% 
  arrange(family) %>% 
  mutate(
    dietg_color = case_when(
      diet_group == 'herbivore' ~ colorz[4],
      diet_group == 'predator' ~ colorz[8],
      is.na(diet_group) ~ colorz[1],
      diet_group == 'omnivore' ~ colorz[3],
      diet_group == 'mixed' ~ colorz[1],
      diet_group == 'scavenger' ~ colorz[7],
      diet_group == 'fungivore' ~ colorz[2]))

# make a foliage plot that can be PCA'ed - columns are families, rows are circles
foliage_base1 <- map_dfc(
  unique(foliage_families$CircleFK),
  ~ all_foliage %>%
    filter(
      CircleFK == .x,
      !is.na(family)) %>% 
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
  cbind(family = unique(all_foliage$family[!is.na(all_foliage$family)])) %>% 
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

foliage_pca <- prcomp(foliage_base1[1:(ncol(foliage_base1)-3)], scale = F)

sub_rot <- foliage_pca$rotation %>% 
  as_tibble(rownames = 'family') %>% 
  filter(abs(PC1) > 0.14 | abs(PC2) > 0.14)%>% 
  left_join(foliage_functions, by = 'family')

foliage_pca_plot <- ggplot() +
  geom_point(
    data = foliage_pca$x,
    mapping = aes(
      x = PC1,
      y = PC2,
      shape = foliage_base1$SiteFK),
    size = 3) +
  geom_segment(
    data = sub_rot,
    mapping = aes(
      x = 0,
      y = 0,
      xend = PC1*15,
      yend = PC2*15,
      color = diet_group),
    arrow = arrow(length = unit(0.03, 'npc')),
    linewidth = 1.1) +
  geom_text_repel(
    data = sub_rot %>% 
      mutate(family = case_when(
        family %in% c('Araneidae','Coccinellidae','Tenebrionidae','Sclerosomatidae') ~ family,
        .default = NULL)),
    mapping = aes(
      x = PC1*15,
      y = PC2*15,
      label = family),
    size = unit(3, 'npc'),
    box.padding = 0.001) +
  scale_color_manual(values = setNames(sub_rot$dietg_color, sub_rot$diet_group)) +
  labs(
    shape = 'Site Code',
    color = 'Functional Group') +
  ul_theme2 +
  theme(plot.margin = unit(c(0.1,0.05,0.05,0.05), unit = 'npc'))

ggsave(
  plot = foliage_pca_plot,
  filename = 'figures/paper/foliage_pca.png',
  width = 6,
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
    x = "\u0394 Herbaceous Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

ground_plot_euclidean_road <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = distanceToRoad,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = "\u0394 Distance to Nearest Road (km)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

ground_plot_euclidean_litter <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = litterDepth,
    y = euclideanDistance)) +
  geom_point() + 
  labs(
    x = "\u0394 Litter Depth (mm)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

ground_plot_euclidean_forest <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = forest_1km,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#009E73') +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 0.5,
    y = 7.5,
    label = 'R2 = 0.48, p < 0.001',
    color = '#009E73') +
  theme(axis.title = element_text(size = 14))

ground_plot_euclidean_distance <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = geographicDistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#E69F00') +
  labs(
    x = "Geographic Distance (km)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 25,
    y = 7.5,
    label = 'R2 = 0.09, p < 0.0005',
    color = '#E69F00') +
  theme(axis.title = element_text(size = 14))

ground_plot_jaccard_herb <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = herbaceousCover,
    y = jaccardDissimilarity)) +
  geom_point() + 
  labs(
    x = "\u0394 Herbaceous Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

ground_plot_jaccard_road <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = distanceToRoad,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = "\u0394 Distance to Nearest Road (km)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

ground_plot_jaccard_litter <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = litterDepth,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = "\u0394 Litter Depth (mm)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

ground_plot_jaccard_forest <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = forest_1km,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#009E73') +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 0.5,
    y = 0.2,
    label = 'R2 = 0.25, p < 0.0005',
    color = '#009E73') +
  theme(axis.title = element_text(size = 14))

ground_plot_jaccard_distance <- ggplot(
  data = ground_dis,
  mapping = aes(
    x = geographicDistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = 'lm',
    se = F,
    color = '#E69F00') +
  labs(
    x = "Geographic Distance (km)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    'text',
    x = 25,
    y = 0.2,
    label = 'R2 = 0.04, p < 0.005',
    color = '#E69F00') +
  theme(axis.title = element_text(size = 14))

ground_euclidean_plots <- ggarrange(
  ground_plot_euclidean_forest, 
  ground_plot_euclidean_road, 
  ground_plot_euclidean_litter,
  ground_plot_euclidean_herb,
  ground_plot_euclidean_distance,
  ncol = 1,
  labels = c('a)','c)','e)','g)','i)'),
  label.x = -0.01)

ground_jaccard_plots <- ggarrange(
  ground_plot_jaccard_forest, 
  ground_plot_jaccard_road, 
  ground_plot_jaccard_litter,
  ground_plot_jaccard_herb,
  ground_plot_jaccard_distance,
  ncol = 1,
  labels = c('b)','d)','f)','h)','j)'),
  label.x = -0.01)

ground_plots <- ggarrange(
  ground_euclidean_plots,
  ground_jaccard_plots,
  ncol = 2)

ggsave(
  filename = 'figures/paper/ground_plots.png',
  plot = ground_plots,
  width = 9.75,
  height = 13.5,
  units = 'in')


# ground arthropod PCA plot -----------------------------------------------

ground_families <- ground_arths %>%
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

all_grounds <- map(
  unique(ground_families$CircleID),
  function(x){
    all_families <-  unique(ground_families$family)
    
    in_families <-  ground_families %>% 
      filter(CircleID == x) %>% 
      pull(family) %>% 
      unique()
    
    out_families <- all_families[!all_families %in% in_families]
    
    rbind(
      ground_families %>% 
        filter(CircleID == x),
      tibble(
        CircleID = rep(x, length(out_families)),
        family = out_families,
        n_individuals = rep(0, length(out_families)),
        biomass = rep(0, length(out_families))))
  }) %>% 
  bind_rows() %>% 
  arrange(CircleID, family)

ground_functions <- ground_families %>% 
  ungroup() %>% 
  filter(!is.na(family)) %>% 
  select(family) %>% 
  distinct() %>% 
  left_join(functions, by = 'family') %>% 
  arrange(family) %>% 
  mutate(
    dietg_color = case_when(
      diet_group == 'herbivore' ~ colorz[4],
      diet_group == 'predator' ~ colorz[8],
      is.na(diet_group) ~ colorz[1],
      diet_group == 'omnivore' ~ colorz[3],
      diet_group == 'mixed' ~ colorz[1],
      diet_group == 'scavenger' ~ colorz[7],
      diet_group == 'fungivore' ~ colorz[2]))

# make a ground plot that can be PCA'ed - columns are families, rows are circles
ground_base1 <- map_dfc(
  unique(ground_families$CircleID),
  ~ all_grounds %>%
    filter(
      CircleID == .x,
      !is.na(family)) %>%
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
  cbind(family = unique(all_grounds$family[!is.na(all_grounds$family)])) %>%
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

ground_pca <- prcomp(ground_base1[1:(ncol(ground_base1)-3)], scale = F)

sub_rot2 <- ground_pca$rotation %>% 
  as_tibble(rownames = 'family') %>% 
  filter(abs(PC1) > 0.14 | abs(PC2) > 0.14)%>% 
  left_join(ground_functions, by = 'family')

ground_pca_plot <- ggplot() +
  geom_point(
    data = ground_pca$x,
    mapping = aes(
      x = PC1,
      y = PC2,
      shape = ground_base1$SiteFK),
    size = 3) +
  geom_segment(
    data = sub_rot2,
    mapping = aes(
      x = 0,
      y = 0,
      xend = PC1*15,
      yend = PC2*15,
      color = diet_group),
    arrow = arrow(length = unit(0.03, 'npc')),
    linewidth = 1.1) +
  geom_text_repel(
    data = sub_rot2 %>% 
      mutate(family = case_when(
        family %in% c('Carabidae','Lycosidae','Armadillidae','Porcellionidae','Rhaphidophoridae') ~ family,
        .default = NULL)),
    mapping = aes(
      x = PC1*15,
      y = PC2*15,
      label = family),
    size = unit(3, 'npc'),
    box.padding = 0.001) +
  scale_color_manual(values = setNames(sub_rot2$dietg_color, sub_rot2$diet_group)) +
  labs(
    shape = 'Site Code',
    color = 'Functional Group') +
  ul_theme2 +
  theme(plot.margin = unit(c(0.1,0.05,0.05,0.05), unit = 'npc'))

ggsave(
  plot = ground_pca_plot,
  filename = 'figures/paper/ground_pca.png',
  width = 6,
  height = 4,
  units = 'in')


# site trait table --------------------------------------------------------

sites <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^sites')])

circles %>% 
  left_join(
    sites,
    by = c('SiteFK' = 'SiteID')) %>% 
  group_by(SiteFK) %>% 
  summarize(
    forest_1km = mean(forest_1km),
    MeanLitter = mean(LitterDepthmm),
    MeanCanopy = mean(PercentCanopyCover),
    MeanHerb = mean(HerbCoverEstimate),
    MeanEdge = mean(DistanceToEdgem)) %>% 
  write_csv('data/site_summaries.csv')
