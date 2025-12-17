wd <- 'C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Albert-Ranas'
wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
setwd(wd)


# Load study area ####
crs = 'EPSG:4326'
study_area_pol <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')
study_area_pol_crs <- st_transform(study_area_pol, crs)
grid <- st_read('shpfiles/grid_03.gpkg')

grid <- st_join(grid, study_area_pol_crs, join = st_intersects)
grid <- grid %>% filter(!is.na(NAME_0))
plot(grid)
##### Load variable (raster; raster to mask; vect)
# lay1 <- rast('shpfiles/treeheight_resampled.tif')
# varName <- 'Tree heigh (m)'

# lay1 <- rast('shpfiles/dem_elevation_WGS84.tif')
# lay1 <- mask(lay1, study_area_pol)
# lay1 <- crop(lay1, study_area_pol)
# varName <- 'Elevation (m)'

lay1 <- vect('shpfiles/grid_clase_2.gpkg')
lay1 <- vect('shpfiles/landCover_grid10/lndCov4.gpkg')
varName <- 'LC:terraFirmTC'

# Load occurrences dataset (3 column format: Species, Longitude, Latitude) ####
data0 <- fread('Data/combinedFrogsrev.csv', sep = ';')
dataGen <- data0[Genetics == 'YES',]
dataNoGen <- data0[Genetics == 'NO',]
data_points <- vect(data0, geom = c("Longitude", "Latitude"),
                    crs = crs)
data_points_gen <- vect(dataGen, geom = c("Longitude", "Latitude"),
                        crs = crs)
data_points_nogen <- vect(dataNoGen, geom = c("Longitude", "Latitude"),
                          crs = crs)

dataLay1 <- terra::extract(lay1, data_points)[, 3]
dataLay1 <- as.data.frame(dataLay1)

dataLay2 <- terra::extract(lay1, data_points_gen)[, 3]
dataLay2 <- as.data.frame(dataLay2)

dataLay3 <- terra::extract(lay1, data_points_nogen)[, 3]
dataLay3 <- as.data.frame(dataLay3)


WS_cent <- read.csv('outputs_knowBR/WS_centroids_comb_01.csv', 
                    sep = ";")
data_points_ws <- vect(WS_cent, geom = c("lon", "lat"),
                    crs = crs)

WS_cent_gen <- read.csv('outputs_knowBR/WS_centroids_comb_01_gen.csv', 
                        sep = ";")
data_points_ws_gen <- vect(WS_cent_gen, geom = c("lon", "lat"),
                           crs = crs)

WS_cent_nogen <- read.csv('outputs_knowBR/WS_centroids_comb_01_nogen.csv', 
                          sep = ";")
data_points_ws_nogen <- vect(WS_cent_nogen, geom = c("lon", "lat"),
                             crs = crs)

dataLay1_ws <- terra::extract(lay1, data_points_ws)[, 3]
dataLay1_ws <- as.data.frame(dataLay1_ws)

dataLay2_ws <- terra::extract(lay1, data_points_ws_gen)[, 3]
dataLay2_ws <- as.data.frame(dataLay2_ws)

dataLay3_ws <- terra::extract(lay1, data_points_ws_nogen)[, 3]
dataLay3_ws <- as.data.frame(dataLay3_ws)

lay1_df <- as.data.frame(lay1)
# functions to plot the freq of each variable 
# for all occurrence dataset and only well surveyd cells
habBiasPlot <- function(varName, title){
  ggplot() +
    geom_density(
      data = lay1_df,
      aes(x = {{varName}},
          y = after_stat(scaled),
          fill = "Overall conditions"),
      alpha = 0.1, linewidth = 0.5,
      colour = "black",
      linetype = "solid") +
      geom_density(
        data = dataLay1,
        aes(x = dataLay1, y = after_stat(scaled),
            fill = "Combined dataset"),
        alpha = 0.1, linewidth = 0.5,
        colour = "maroon",
        linetype = "solid"
      ) +
      geom_density(
        data = dataLay2,
        aes(x = dataLay2, y = after_stat(scaled),
            fill = "Combined DS with genetic info"),
        alpha = 0.1, linewidth = 0.5,  
        colour = "orange",
        linetype = "solid"
      ) +
      geom_density(
        data = dataLay3,
        aes(x = dataLay3, y = after_stat(scaled),
            fill = "Combined DS without genetic info"),
        alpha = 0.1, linewidth = 0.5, 
        colour = "purple",
        linetype = "solid"
      ) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "Overall conditions" = "#CDC0B0",
          "Combined dataset" = "maroon",
          "Combined DS with genetic info" = "orange",
          "Combined DS without genetic info" = "purple"
        )) +
      ylab('Freq') + xlab(title) +
      theme_minimal()
}
habBiasPlotWS <- function(varName, title){
  ggplot() +
  geom_density(
    data = lay1_df,
    aes(x = {{varName}},
        y = after_stat(scaled),
        fill = "Overall conditions"),
    alpha = 0.1, linewidth = 0.5,
    colour = "black",
    linetype = "solid"
  ) +
  geom_density(
    data = dataLay1_ws,
    aes(x = dataLay1_ws, y = after_stat(scaled),
        fill = "WS cells - Combined dataset"),
    alpha = 0.1, linewidth = 0.5,
    colour = "maroon",
    linetype = "dashed"
  ) +
  geom_density(
    data = dataLay2_ws,
    aes(x = dataLay2_ws, y = after_stat(scaled),
        fill = "WS cells - Combined dataset with genetic info"),
    alpha = 0.1, linewidth = 0.5,
    colour = "orange",
    linetype = "dashed"
  ) +
  geom_density(
    data = dataLay3_ws,
    aes(x = dataLay3_ws, y = after_stat(scaled),
        fill = "WS cells - Combined dataset without genetic info"),
    alpha = 0.1, linewidth = 0.5,
    colour = "purple",
    linetype = "dashed"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Overall conditions" = "#CDC0B0",
      "WS cells - Combined dataset" = "maroon",
      "WS cells - Combined dataset with genetic info" = "orange",
      "WS cells - Combined dataset without genetic info" = "purple"
    )) +
  ylab('Freq') + xlab(title) +
  theme_minimal()
}

# plot and save:
var_field <- names(lay1_df)[2]
habBiasPlot(var_field, varName)

ggsave(paste0(varName, "desc_Comparison.png"), plot = last_plot(), 
       device = "png",
       width = 8, height = 3, units = "in", dpi = 600)


habBiasPlotWS(var_field, varName)
ggsave(paste0(varName, "descWS_Comparison.png"), plot = last_plot(), 
       device = "png",
       width = 10, height = 3, units = "in", dpi = 600)

##### kolmogorov smirnov test ####
results_ks <- matrix(nrow = 6, ncol = 2)
colnames(results_ks) <- c("Statistic", "P-value")
rownames(results_ks) <- paste0("Test_", 1:6)

list2 <- c('dataLay1$dataLay1',
           'dataLay2$dataLay2',
           'dataLay3$dataLay3',
           'dataLay1_ws$dataLay1_ws',
           'dataLay2_ws$dataLay2_ws',
           'dataLay3_ws$dataLay3_ws'
)
i = 1
for(j in seq_along(list2)){
  parts <- strsplit(list2[3], "\\$")[[1]]
  df_name <- parts[1]
  col_name <- parts[2]
  
  y_data <- get(df_name)[[col_name]]
  ks1 <- ks.test(lay1_df[,1], y_data)
  results_ks[i,1] <- round(ks1$statistic, 3)
  results_ks[i,2] <- round(ks1$p.value, 3)
  i = i+1
}

write.table(results_ks, paste0(varName, "comb_ks.txt"), sep = "\t", 
            row.names = FALSE, quote = FALSE)

##### kruskal wallis test ####
results_kw <- matrix(nrow = 6, ncol = 2)
colnames(results_kw) <- c("Statistic", "P-value")
rownames(results_kw) <- paste0("Test_", 1:6)

# X axis is a probability density
# Kruskal-Wallis verifies whether 1) the distribution of well-sampled sites
# is an unbiased subset of the entire habitat conditions.
# If this is so, p > 0.05
# Kruskal-Wallis and kolmogorov Smirnov tests ####
i = 1
  for(j in seq_along(list2)){
    parts <- strsplit(list2[i], "\\$")[[1]]
    df_name <- parts[1]
    col_name <- parts[2]
    
    y_data <- get(df_name)[[col_name]]
    x_axis <- c(lay1_df[,1], y_data)
    g_axis <- as.factor(c(rep("area", length(lay1_df[,1])),
                          rep("occ", length(y_data))))
    kw <- kruskal.test(x_axis ~ g_axis)
    results_kw[i,1] <- round(kw$statistic, 3)
    results_kw[i,2] <- round(kw$p.value, 3)
    i = i+1
  }
write.table(results_kw, paste0(varName, "COMB_kw.txt"), sep = "\t", 
            row.names = FALSE, quote = FALSE)

