
## setup -------------------------------------------------------------------

# load necessary packages
library(tidyverse)
library(raster)
library(rgdal)
library(corrplot)

# read in the raster file of land cover classes in the study area
nlcd <- raster('data/nlcd_local') %>% 
  # the raster seems to have picked up a weird extra chunk of extent on the right edge - cropping it out
  crop(extent(c(1493025,1551500,1541175,1598865)))

# read in the sampling plot info
circles <- read_csv("data/circles_2022-10-03.csv")

dissim <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^foliage_dissimilarity")])

# assign coordinate system to the sampling plots
coordinates(circles) <- c("Longitude","Latitude")

# assign projection to the sampling plots
proj4string(circles) <- CRS("+proj=longlat +datum=WGS84")

# convert the coordinates of sampling plots to match the NLCD file
circlesUTM <- spTransform(circles, crs(nlcd))

# plot the raster and sampling plots to verify
raster::plot(nlcd)
points(x = circlesUTM@coords[,1], y = circlesUTM@coords[,2], pch = 16, cex = 0.5)

# create a patch raster for sampling plots where pixels containing a sample plot are assigned a value of 1 and all others are assigned a value of 0
circles_raster <- nlcd %>% 
  setValues(0)

circles_raster[cellFromXY(circles_raster, circlesUTM)] <- 1

circlesUTM$rastercell <- cellFromXY(circles_raster, circlesUTM)

circlesUTM %>% 
  as_tibble() %>% 
  cbind(nodeID = c(11:15,21:24,26,31:33,35,38,41,44:46,48,52:53,59,510,511,63:64,68,613,614)) %>% 
  dplyr::select(nodeID, Longitude, Latitude) %>% 
  write.table(
    file = "data/circles.txt",
    col.names = F,
    row.names = F)

## create resistance surfaces ----------------------------------------------


# all10 -------------------------------------------------------------------

# all forest is assigned resistance of 1, all other classes assigned resistance of 10
# create a reclassification table
all10 <- tibble(
  from = unique(nlcd),
  to = c(rep(10,6),rep(1,3),rep(10,6)))

# reclassify the raster
nlcd_all10 <- reclassify(x = nlcd, rcl = all10)

# write the raster
writeRaster(nlcd_all10, filename = "data/all10", format = "ascii", overwrite = T)


# mod1 --------------------------------------------------------------------

# water, barren, all developed except open space assigned 10, all non-forest plant cover (including agriculture and developed open space) assigned 5, all forest assigned 1

mod1 <- tibble(
  from = unique(nlcd),
  to = c(10,5,rep(10,4),rep(1,3),rep(5,6)))

nlcd_mod1 <- reclassify(x = nlcd, rcl = mod1)

writeRaster(nlcd_mod1, filename = "data/mod1", format = "ascii", overwrite = T)


# mod2 --------------------------------------------------------------------

# open water = 9, developed = 7-10 by class, barren = 9, forest = 1, shrub and grassland = 2, agriculture = 7, wetlands = 3

mod2 <- tibble(
  from = unique(nlcd),
  to = c(9,7:10,9,3,3,3,2,2,7,7,3,3))

nlcd_mod2 <- reclassify(x = nlcd, rcl = mod2)

writeRaster(nlcd_mod2, filename = "data/mod2", format = "ascii", overwrite = T)


## calculate covariance in resistance distances across the three models and geographic distance --------

paths <- read_table(
  file = "data/all10_paths_resistances_3columns",
  col_names = F) %>% 
  rename("all10_resistance" = X3) %>% 
  full_join(
    read_table(
      file = "data/mod1_paths_resistances_3columns",
      col_names = F) %>% 
      rename("mod1_resistance" = X3),
    by = c("X1","X2")) %>% 
  full_join(
    read_table(
      file = "data/mod2_paths_resistances_3columns",
      col_names = F) %>% 
      rename("mod2_resistance" = X3),
    by = c("X1","X2")) %>% 
  rename("nodeID_1" = X1, "nodeID_2" = X2) %>% 
  mutate(
    circle1 = str_replace(nodeID_1, "^1", "DF") %>% 
      str_replace("^2", "ERSP") %>% 
      str_replace("^3", "JMNP") %>% 
      str_replace("^4", "NCBG") %>% 
      str_replace("^5", "NCSU") %>% 
      str_replace("^6", "UNC"),
    circle2 = str_replace(nodeID_2, "^1", "DF") %>% 
      str_replace("^2", "ERSP") %>% 
      str_replace("^3", "JMNP") %>% 
      str_replace("^4", "NCBG") %>% 
      str_replace("^5", "NCSU") %>% 
      str_replace("^6", "UNC"),
    circles = str_c(circle1, circle2, sep = "_")) %>% 
  full_join(
    dissim %>% 
      dplyr::select(circles, geographicDistance),
    by = "circles") %>% 
  dplyr::select(!c(nodeID_1,nodeID_2,circle1,circle2)) %>% 
  relocate(circles)


pMatrix <- function(mat, ...){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p_vals <- pMatrix(paths[2:5])

paths[2:5] %>% 
  cor(use = 'complete.obs') %>%
  corrplot(
    method = 'circle',
    type = 'upper',
    tl.col = 'black',
    tl.cex = 0.6,
    p.mat = p_vals,
    sig.level = 0.1,
    insig = 'blank')

ggplot(
  data = paths,
  mapping = aes(
    x = geographicDistance,
    y = mod1_resistance)) +
  geom_point() +
  geom_smooth(method = "lm")
