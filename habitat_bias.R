wd <- 'C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Albert-Ranas'
wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
setwd(wd)


# Load study area ####
crs = 'EPSG:4326'
study_area_pol <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')
study_area_pol_crs <- st_transform(study_area_pol, crs)
grid <- st_read('shpfiles/grid_01.gpkg')

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

lay1 <- vect('shpfiles/landCover_grid10/lndCov2.gpkg')
lay1_df <- as.data.frame(lay1) 
varName <- 'LC_TFsemiarid'
var_field <- names(lay1_df)[2]
lay1_df <- lay1_df %>% filter(clase_2 != 0)
# Load occurrences dataset (3 column format: Species, Longitude, Latitude) ####
data0 <- fread('Data/combinedFrogsrev.csv', sep = ';') #FrogsDescribedrev
dataGen <- data0[Genetics == 'YES',]
dataNoGen <- data0[Genetics == 'NO',]
data_points <- vect(data0, geom = c("Longitude", "Latitude"),
                    crs = crs)
data_points_gen <- vect(dataGen, geom = c("Longitude", "Latitude"),
                        crs = crs)
data_points_nogen <- vect(dataNoGen, geom = c("Longitude", "Latitude"),
                          crs = crs)
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

dataLay1 <- terra::extract(lay1, data_points)[, 3]
dataLay1 <- as.data.frame(dataLay1)

dataLay2 <- terra::extract(lay1, data_points_gen)[, 3]
dataLay2 <- as.data.frame(dataLay2)

dataLay3 <- terra::extract(lay1, data_points_nogen)[, 3]
dataLay3 <- as.data.frame(dataLay3)

dataLay1_ws <- terra::extract(lay1, data_points_ws)[, 3]
dataLay1_ws <- as.data.frame(dataLay1_ws)

dataLay2_ws <- terra::extract(lay1, data_points_ws_gen)[, 3]
dataLay2_ws <- as.data.frame(dataLay2_ws)

dataLay3_ws <- terra::extract(lay1, data_points_ws_nogen)[, 3]
dataLay3_ws <- as.data.frame(dataLay3_ws)

# mode and median of each dataset ####
calc_stats <- function(df, var_name = NULL) {
  # Median
  median_val <- median(df, na.rm = TRUE)
  # Mode
  dens <- density(df, na.rm = TRUE)
  mode_val <- dens$x[which.max(dens$y)]
  
  return(list(median = median_val, mode = mode_val))
}

stats_lay1 <- calc_stats(lay1_df$clase_2)
stats_data1 <- calc_stats(dataLay1$dataLay1)
stats_data2 <- calc_stats(dataLay2$dataLay2)
stats_data3 <- calc_stats(dataLay3$dataLay3)

stats_dataws1 <- calc_stats(dataLay1_ws$dataLay1_ws)
stats_dataws2 <- calc_stats(dataLay2_ws$dataLay2_ws)
stats_dataws3 <- calc_stats(dataLay3_ws$dataLay3_ws)
# functions to plot the freq of each variable 
# for all occurrence dataset and only well surveyd cells
habBiasPlot <- function(var_field, title){
  ggplot() +
    geom_density(
      data = lay1_df,
      aes(x = .data[[var_field]],
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
    # geom_vline(xintercept = stats_lay1$median, 
    #            color = "black", linetype = "solid", linewidth = 1) +
    # geom_vline(xintercept = stats_data1$median, 
    #            color = "maroon", linetype = "solid", linewidth = 1) +
    # geom_vline(xintercept = stats_data2$median, 
    #            color = "orange", linetype = "solid", linewidth = 1) +
    # geom_vline(xintercept = stats_data3$median, 
    #            color = "purple", linetype = "solid", linewidth = 1) +
    # 
    # 
    # geom_vline(xintercept = stats_lay1$mode, 
    #            color = "black", linetype = "dashed", linewidth = 0.5) +
    # geom_vline(xintercept = stats_data1$mode, 
    #            color = "maroon", linetype = "dashed", linewidth = 0.5) +
    # geom_vline(xintercept = stats_data2$mode, 
    #            color = "orange", linetype = "dashed", linewidth = 0.5) +
    # geom_vline(xintercept = stats_data3$mode, 
    #            color = "purple", linetype = "dashed", linewidth = 0.5) +
    # 
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
habBiasPlotWS <- function(var_field, title){
  ggplot() +
  geom_density(
    data = lay1_df,
    aes(x = .data[[var_field]],
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
    # geom_vline(xintercept = stats_lay1$median, 
    #            color = "black", linetype = "solid", linewidth = 0.5) +
    # geom_vline(xintercept = stats_dataws1$median, 
    #            color = "maroon", linetype = "solid", linewidth = 0.5) +
    # geom_vline(xintercept = stats_dataws2$median, 
    #            color = "orange", linetype = "solid", linewidth = 0.5) +
    # geom_vline(xintercept = stats_dataws3$median, 
    #            color = "purple", linetype = "solid", linewidth = 0.5) +
    # 
    # # Líneas verticales para moda (línea discontinua)
    # geom_vline(xintercept = stats_lay1$mode, 
    #            color = "black", linetype = "dashed", linewidth = 0.5) +
    # geom_vline(xintercept = stats_dataws1$mode, 
    #            color = "maroon", linetype = "dashed", linewidth = 0.5) +
    # geom_vline(xintercept = stats_dataws2$mode, 
    #            color = "orange", linetype = "dashed", linewidth = 0.5) +
    # geom_vline(xintercept = stats_dataws3$mode, 
    #            color = "purple", linetype = "dashed", linewidth = 0.5) +
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
habBiasPlot(var_field, varName)

ggsave(paste0(varName, "_comb_Comparison.png"), plot = last_plot(), 
       device = "png",
       width = 8, height = 3, units = "in", dpi = 600)

habBiasPlotWS(var_field, varName)
ggsave(paste0(varName, "combWS_Comparison.png"), plot = last_plot(), 
       device = "png",
       width = 10, height = 3, units = "in", dpi = 600)

##### kolmogorov smirnov test ####
list2 <- c('dataLay1$dataLay1',
           'dataLay2$dataLay2',
           'dataLay3$dataLay3',
           'dataLay1_ws$dataLay1_ws',
           'dataLay2_ws$dataLay2_ws',
           'dataLay3_ws$dataLay3_ws'
)

results_ks <- matrix(nrow = 6, ncol = 2)
colnames(results_ks) <- c("Statistic", "P-value")
rownames(results_ks) <- paste0("Test_", 1:6)

for(j in seq_along(list2)){
  parts <- strsplit(list2[j], "\\$")[[1]]
  df_name <- parts[1]
  col_name <- parts[2]
  
  y_data <- get(df_name)[[col_name]]
  ks1 <- ks.test(lay1_df[,2], y_data)
  results_ks[j,1] <- round(ks1$statistic, 3)
  results_ks[j,2] <- round(ks1$p.value, 3)

}
results_ks
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
  for(j in seq_along(list2)){
    parts <- strsplit(list2[j], "\\$")[[1]]
    df_name <- parts[1]
    col_name <- parts[2]
    
    
    y_data <- get(df_name)[[col_name]]
    x_axis <- c(lay1_df[,2], y_data)
    g_axis <- as.factor(c(rep("area", length(lay1_df[,2])),
                          rep("occ", length(y_data))))
    kw <- kruskal.test(x_axis ~ g_axis)
    results_kw[j,1] <- round(kw$statistic, 3)
    results_kw[j,2] <- round(kw$p.value, 3)
  }

results_kw
write.table(results_kw, paste0(varName, "desc_kw.txt"), sep = "\t", 
            row.names = FALSE, quote = FALSE)

