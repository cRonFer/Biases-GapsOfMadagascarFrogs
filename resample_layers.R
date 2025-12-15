wd <- 'C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Albert-Ranas'
wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
setwd(wd)

library(sf)
library(ggplot2)
library(terra)
library(tidyterra)

# Load study area ####
crs = 4326
area <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')
area <- st_transform(area, crs)
grid <- st_read('shpfiles/grid_01.gpkg')
####creacion capas habitat coverage ####
### raster climatico como referencia
clim_ras <- rast('C:/Users/MNCN-JHICA/Downloads/chelsa/chelsa_bio1_1981_2010_WGS84.tif')
raster_mask <- mask(clim_ras, area)
raster_mask <- crop(raster_mask, area)
# Crear un raster vacío con la estructura EXACTA del original
raster_plantilla <- rast(ext(raster_mask), 
                         resolution = res(raster_mask),
                         crs = crs(raster_mask))

### land type
land <- vect('landType.gpkg')
plot(land)

rasterizado <- rasterize(land, raster_mask, field = "CODE")
plot(rasterizado)
levels(rasterizado)
writeRaster(rasterizado, 'landType.tif')
### vegetation
veg <- rast('shpfiles/moatsmith_1km.tif')
raster_resampleado <- resample(x = veg,   # Tu raster original
                               y = raster_mask,   # La capa de referencia
                               method = "modal")         # Método para categorías
plot(raster_resampleado)
writeRaster(raster_resampleado, 'veg_resampled.tif')

### forest
forest <- rast('C:/Users/MNCN-JHICA/Downloads/forbin.tif')
forest_resampleado <- resample(x = forest,   # Tu raster original
                               y = raster_mask,   # La capa de referencia
                               method = "near")         # Método para categorías
plot(forest_resampleado)
writeRaster(forest_resampleado, 'forest_resampled.tif')


## tree height
tree_height <- rast('tree_height.tif')
treeh_resampleado <- resample(x = tree_height,   # Tu raster original
                              y = raster_mask,   # La capa de referencia
                              method = "mean")         # Método para categorías
plot(treeh_resampleado)
writeRaster(treeh_resampleado, 'treeheight_resampled.tif')

### land cover reclass and resampled
lcov <- rast('C:/Users/MNCN-JHICA/Downloads/landUse_combined_MDG.tif')
lcov_classes <- read.csv('C:/Users/MNCN-JHICA/Desktop/proyectoInveCov/land_cover_classes.csv', sep = ';')
lcov_classes <- lcov_classes[, -2]
# matriz_rangos <- matrix(c(
#   0, 1, 1,    # Terra Firma true desert
#   2, 18, 2,    # Terra Firma semi arid
#   19, 24, 3,    # Terra Firma Dense short vegetation
#   25, 96, 4,   # Terra Firma Tree
#   100, 101, 5, # wetland salt pan
#   102, 118, 6, # wetland sparse vegetation
#   119, 124, 7, # wetland sense short veg
#   125, 196, 8, # wetland tree cover
#   200, 211, 9, # Open surface water
#   240, 240, 10, # Short vegetation
#   241, 243, 11, # snow ice
#   244, 247, 12, #cropland
#   248, 249, 13,  # Cropland loss
#   250, 253, 14, # built in
#   254, 254, 15, # ocean
#   255, 255, 26
# ), ncol = 3, byrow = TRUE)

lcov_reclas <- classify(lcov, lcov_classes)
lcov_mask <- mask(lcov_reclas, area)
lcov_mask <- crop(lcov_mask, area)

plot(lcov_mask)
writeRaster(lcov_mask, 'landUse_reclassif2.tif')

lcov_resamp <- resample(x = lcov_mask,   # Tu raster original
                               y = raster_mask,   # La capa de referencia
                               method = "modal")         # Método para categorías

####
lcov_mask <- rast('shpfiles/landUse_reclassif2.tif')
lcov_extr <- extract(lcov_mask, grid)

# Calcular porcentajes por celda del grid
porcentajes <- lcov_extr %>%
  group_by(ID) %>%
  count(landUse_combined_MDG) %>%
  mutate(porcentaje = (n / sum(n)) * 100) %>%
  ungroup() %>%
  select(ID, clase = landUse_combined_MDG, porcentaje)

# 4. Convertir a formato wide para una tabla por grid
porcentajes_wide <- porcentajes %>%
  pivot_wider(
    id_cols = ID,
    names_from = clase,
    values_from = porcentaje,
    names_prefix = "clase_",
    values_fill = 0
  )

# 5. Unir con el grid original
grid_resultado <- grid %>%
  mutate(ID = row_number()) %>%
  left_join(porcentajes_wide, by = "ID")


# Crear capas separadas por clase
for(class in 4:21) {
  grid_lyr <- grid_resultado[,c(1,class)]
  st_write(grid_lyr, paste0("grid_clase_", class, ".gpkg")
  )
}

### vegetation
veg_extr <- extract(veg, grid)

# Calcular porcentajes por celda del grid
perc <- veg_extr %>%
  group_by(ID) %>%
  count(moatsmith_1km) %>%
  mutate(porcentaje = (n / sum(n)) * 100) %>%
  ungroup() %>%
  select(ID, clase = moatsmith_1km, porcentaje)

# 4. Convertir a formato wide para una tabla por grid
perc_wide <- perc %>%
  pivot_wider(
    id_cols = ID,
    names_from = clase,
    values_from = porcentaje,
    names_prefix = "clase_",
    values_fill = 0
  )

# 5. Unir con el grid original
grid_resultado <- grid %>%
  mutate(ID = row_number()) %>%
  left_join(perc_wide, by = "ID")


# Crear capas separadas por clase
for(class in 3:11) {
  grid_lyr <- grid_resultado[,c(1,class)]
  st_write(grid_lyr, paste0("veg_grid10_", class, ".gpkg")
  )
}
