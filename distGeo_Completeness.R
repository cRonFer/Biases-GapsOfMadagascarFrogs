library(geosphere)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
wd <- 'C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Movidas/package/inveCov/inveCov'
wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
setwd(wd)
# Load study area
study_area_pol <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')
grid <- st_read('shpfiles/grid_01.gpkg')
grid <- st_join(grid, study_area_pol, join = st_intersects)
grid <- grid %>% 
  filter(!is.na(NAME_0))
plot(grid)

grid_spatial <- st_transform(grid, crs = 4326)
grid_spatial <- sf::as_Spatial(grid_spatial)
grid_ea <- grid_spatial
# datos
est <- read.csv('C:/Users/MNCN-JHICA/Desktop/proyectoInveCov/outputs_knowBR/combined_01/Estimators.CSV', header = TRUE, sep = ",")
# Identificar celdas bien muestreadas (>75) y las otras
comp_shp <- merge(grid, est, by.x = 'id', by.y = 'Area', all.x = TRUE) # Write here 'by.x' = the unique identifier name of your grid shapefile
comp_shp_cent <- st_centroid(comp_shp)

comp_shp_cent <- comp_shp_cent %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) 


estWS <- comp_shp_cent %>% filter(Records >= 30) %>%
  filter(Ratio >= 5) %>%
  filter(Slope <= 0.05) %>%
  filter(Completeness >= 75)
estWS <- data.frame(estWS)
ws_ids <- estWS$id
estNOws <- comp_shp_cent[!comp_shp_cent$id %in% ws_ids, ]
estNOws <- data.frame(estNOws)
estNOws$Completeness[is.na(estNOws$Completeness)] <- 0
# Función usando distancia inversa ponderada por completitud
#  que da más peso a las celdas cercanas.
calcular_peso_alternativo <- function(celda, estWS, poder = 2) {
  # Calcular distancias
  distancias <- distGeo(
    cbind(celda$lon, celda$lat),
    cbind(estWS$lon, estWS$lat)
  )
  # Convertir a km y evitar ceros
  distancias_km <- pmax(distancias / 1000, 1)
  # Completitudes de celdas buenas
  completitudes_buenas <- estWS$Completeness
  # Ponderación por distancia inversa (weighted inverse distance)
  # La distancia tiene un poder (generalmente 1, 2 o 3)
  pesos_distancia <- 1 / (distancias_km^poder)
  # Normalizar pesos de distancia
  pesos_distancia_norm <- pesos_distancia / sum(pesos_distancia)
  # Combinar con completitudes
  influencia_total <- sum(completitudes_buenas * pesos_distancia_norm)
  # Peso final considerando la completitud propia
  peso_final <- celda$Completeness * influencia_total / 100
  return(peso_final)
}

# Calcular con el método alternativo
estNOws$peso_alternativo <- sapply(1:nrow(estNOws), function(i) {
  calcular_peso_alternativo(estNOws[i, ], estWS, poder = 2)
})

grid_pesos <- merge(grid, estNOws, by.x = 'id', by.y = 'id', 
                    all.x = TRUE) # Write here 'by.x' = the unique identifier name of your grid shapefile
# Crear un nuevo objeto sf solo con los polígonos
grid_simple <- st_as_sf(data.frame(
  id = grid_pesos$id,
  peso_alternativo = grid_pesos$peso_alternativo,
  Completeness = grid_pesos$Completeness,
  geometry = grid_pesos$geom.x  # Usar solo los polígonos
))

# Verificar
st_geometry_type(grid_simple)

plot(grid_simple["peso_alternativo"],
     main = "Pesos alternativos por celda",
     pal = heat.colors,  # Paleta de colores
     key.pos = 4,        # Posición de la leyenda (1=abajo, 2=izq, 3=arriba, 4=der)
     axes = TRUE)

# Gráfico simple de la relación
plot(estNOws$Completeness, estNOws$peso_alternativo,
     xlab = "Completeness original (1-75)",
     ylab = "Peso normalizado (0-100)",
     main = "Relación Completeness-Peso",
     pch = 19, col = "blue")
abline(lm(peso_alternativo ~ Completeness, data = estNOws), col = "red")

write.csv(estNOws, 'estNOws.csv')
