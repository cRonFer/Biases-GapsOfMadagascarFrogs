wd <- 'C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Albert-Ranas'
wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
setwd(wd)
### FIRST RUN SCRIPTS  - function_loadpackages, 
                        # function_create_set_wd 

# Study area shpfiles ####
  # Load study area
  crs = 'epsg:4326'
  study_area_pol <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')
  study_area_pol_crs <- st_transform(study_area_pol, crs)
  data(adworld) # knowBR needs add world polygon to work
  gridResName <- '_07'
  grid <- st_read('shpfiles/grid_MDG75.gpkg') # grid_01 or grid_03
  
  grid <- st_join(grid, study_area_pol, join = st_intersects)
  grid <- grid %>% 
    filter(!is.na(NAME_0))
  plot(grid)
  
  grid_spatial <- st_transform(grid, crs = crs)
  grid_spatial <- sf::as_Spatial(grid_spatial)
  grid_ea <- grid_spatial
  est_sum <- grid[0, ]
  
# Occurrence records ####
  # Load occurrences dataset (3 column format: Species, Longitude, Latitude)
  data <- fread('Data/FrogsDescribedrev.csv', sep = ';') # combinedFrogsrev.csv
  dtName <- 'desc' #'comb'
  # transform into spatial object
  data_points <- vect(data, geom = c("Longitude", "Latitude"), 
       crs = crs)

  # Filter dataset to only records with consensus species name:
  data <- data[!is.na('Species'), ]
  # Add field of abundance for knowBR
  data$Counts <- 1

  # Plot of study area + grid + occurrence records subsets
 map1 <- ggplot() +
      geom_sf(data = study_area_pol, fill = 'lightgrey', color = "lightgrey", 
              linewidth = 0.7) +
      # geom_sf(data = grid, fill = "transparent", color = "black") +
      geom_sf(data = data_points %>% filter(Genetics == 'YES'), color = 'purple') +
      coord_sf(crs = crs) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = 'white', color = 'white'))
 
 map2 <- ggplot() +
   geom_sf(data = study_area_pol, fill = 'lightgrey', color = "lightgrey", 
           linewidth = 0.7) +
   # geom_sf(data = grid, fill = "transparent", color = "black") +
   geom_sf(data = data_points %>% filter(Genetics == 'NO'), color = 'orange') +
   coord_sf(crs = crs) +
   theme_minimal() +
   theme(plot.background = element_rect(fill = 'white', color = 'white')) 
 
  map1|map2
  ggsave('map_points_describedFrogs.png', plot = last_plot(), 
         width = 10, height = 10, dpi = 600)
  
# Inventory completeness analysis ####
  dir_e = paste0(dtName, gridResName)
  create_and_set_directory(dir_e)
    # Well-surveyed criteria:
  NRecords_WS <- 50
  Ratio_WS <- 5
  Slope_WS <- 0.02
  Completeness_WS <- 75
  # Plot completeness estimators for establishing threshold of well-surveyed cells ----
  threshold_plot <- function(var, x, xtitle){
    ggplot(est) +
      geom_point(aes(var, Completeness), pch = 19, size = 1) +
      geom_vline(xintercept = x, col = 'red3', lwd = 1, lty = 2) +
      geom_hline(yintercept = 75, col = 'grey', lwd = 1, lty = 2) + # Here completeness threshold =70%
      theme_minimal() + 
      ylab('') +
      xlab(xtitle) +
      theme(strip.text.y = element_blank(),
            axis.text = element_text(size = 10, face = "bold"),
            axis.title = element_text(size = 10, face = "bold"))
  }
  # All occurrences ####
  colnames(data)[1] <- 'species'
  data <- data[,c(1,3,2,4,5)]
  data0 <- data[,-4]
  
  KnowBPolygon(data = data0, shape = grid_spatial,
               admAreas = FALSE,  # Use predefined grid as personalized polygons
               shapenames = "id", Maps = FALSE,
               dec = ".")
  # Check the estimators from knowBR output:
  est <- read.csv('Estimators.CSV', header = TRUE, sep = ",")
  
  # Distribution of completeness values by other estimators:
  p1 <- threshold_plot(est$Records, NRecords_WS, 'Records') + ylab('Completeness')
  p2 <- threshold_plot(est$Ratio, Ratio_WS, 'Ratio')
  p3 <- threshold_plot(est$Slope, Slope_WS, 'Slope')
  completn_thresholds <- grid.arrange(p1, p2, p3, ncol = 3)
  ggsave('completeness_thresholds.png', plot = completn_thresholds,
         width = 12, height = 5, bg = "white", dpi = 600)
  
  # plot completeness values study area map
  comp_shp <- merge(grid, est, by.x = 'id', # by.x identifier name of your grid shapefile
                    by.y = 'Area', #by.yidentifier name of estimators file
                    all.x = TRUE) 
  comp_shp <- comp_shp %>%
                      filter(!is.na(Completeness)) %>%
                      st_transform(crs)
  
  # Filter est dataset selecting WELL-SURVEYED CELLS:
  estWS <- comp_shp %>% filter(Records >= NRecords_WS) %>%
                        filter(Ratio >= Ratio_WS) %>%
                        filter(Slope <= Slope_WS) %>%
                        filter(Completeness >= Completeness_WS)
                     
  print(paste('There are ', nrow(estWS), 
              ' well surveyed cells in the study area based on the chosen thresholds'))
  # Map of Completeness
  ggplot() +
    geom_sf(data = study_area_pol, fill = 'lightgrey', color = 'darkgrey') +
    geom_sf(data = comp_shp, aes(fill = Completeness), color = "transparent") +
    geom_sf(data = estWS, fill = 'transparent', color = "black", linewidth = 1) +
    
    scale_fill_viridis_c(limits = c(0, 100), option = "plasma", name = "",
                         direction = -1, alpha = 0.6) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
  
  ggsave('wellSurveyCells_Map.png', plot = last_plot(), 
         dpi = 500, width = 6, height = 8, bg = 'white')
  
  # Extract centroids of Well Survey cells for the environmental Space analysis
  WS_cent <- st_centroid(estWS)
  
  WS_cent_dt <- WS_cent %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                          lat = sf::st_coordinates(.)[,2]) %>%
                            dplyr::select(c(id, lon, lat))
  WS_cent_dt$geometry <- NULL
  fwrite(WS_cent_dt, 'WS_centroids.csv', sep = ";")
  
  # Occurrences with genetic information ####
  sbsetName <- '_gen'
  setwd(wd)
  dir_e = paste0(dtName, gridResName, sbsetName)
  create_and_set_directory(dir_e)
  data1 <- data %>% 
               filter(Genetics == 'YES') %>% 
                  select(c(1:3,5))
  
  KnowBPolygon(data = data1, shape = grid_spatial,
               admAreas = FALSE,  # Use predefined grid as personalized polygons
               shapenames = "id", Maps = FALSE,
               dec = ".")
#### REPEAT FROM LINE 90
  # Occurrences without genetic information####
  sbsetName <- '_nogen'
  setwd(wd)
  dir_e = paste0(dtName, gridResName, sbsetName)
  create_and_set_directory(dir_e)
  
  data2 <- data %>% 
              filter(Genetics == 'NO') %>% 
                select(c(1:3,5))
  
  KnowBPolygon(data = data2, shape = grid_spatial,
               admAreas = FALSE,  # Use predefined grid as personalized polygons
               shapenames = "id", Maps = FALSE,
               dec = ".")
  #### REPEAT FROM LINE 90
  
# Plot maps of WS cells comparion between datasets ####
grid_spatial_ws <- grid
WS_cent_dt <- fread(paste0(wd, '/described_07/WS_centroids.csv'), sep = ';')
WS_cent_dt <- WS_cent_dt$id
grid_spatial_ws <- grid_spatial_ws[grid_spatial_ws$id %in% 
                                     WS_cent_dt,]

WS_cent_dt_gen <- fread(paste0(wd, '/described_07_gen/WS_centroids.csv'), sep = ';')
WS_cent_dt_gen <- WS_cent_dt_gen$id
grid_spatial_ws_gen <- grid
grid_spatial_ws_gen <- grid_spatial_ws_gen[grid_spatial_ws_gen$id %in% 
                                             WS_cent_dt_gen,]

WS_cent_dt_nogen <- fread(paste0(wd, '/described_07_nogen/WS_centroids.csv'), sep = ';')
WS_cent_dt_nogen <-WS_cent_dt_nogen$id
grid_spatial_ws_nogen <- grid
grid_spatial_ws_nogen <- grid_spatial_ws_nogen[grid_spatial_ws_nogen$id %in% 
                                                 WS_cent_dt_nogen,]

setwd(wd)
ggplot() +
    geom_sf(data = study_area_pol, fill = 'lightgrey', color = 'darkgrey') +
  
    geom_sf(data = grid_spatial_ws_nogen,
            color = "transparent", fill = 'orange',linewidth = 0.1) +
    
    geom_sf(data = grid_spatial_ws_gen,
            color = "transparent", fill = 'purple', alpha = 0.5, linewidth = 0.1) +
  
    geom_sf(data = grid_spatial_ws,
            color = "black", fill = 'transparent', linewidth = 0.1) +
    
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white",
                                           color = "transparent"),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
    
ggsave(paste0(dtName, gridResName, 'comparisonWS_cells_Map.png'), 
       plot = last_plot(), dpi = 600, bg = 'white',
       width = 6, height = 8)


