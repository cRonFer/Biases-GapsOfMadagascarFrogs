
wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
# wd <- ('C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/')
setwd(wd)

dir_e = 'sampBias_outputs'
create_and_set_directory(dir_e)

crs_standard <- "EPSG:4326"
area <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')

# Select and download raster files of variables from rnaturalearth ####
gaz <- list()
roads <- vect('naturalEarth/MDG_Roads.gpkg')
gaz[[1]] <- roads
names(gaz)[[1]] <- 'Roads'
crs(gaz$Roads) <- crs_standard

pplaces <- vect('naturalEarth/MDG_populated_places.gpkg')
gaz[[2]] <- pplaces
names(gaz)[[2]] <- 'Populated places'
crs(gaz[[2]]) <- crs_standard

#### Load extra layers #### 
# rivers
rv <- vect('naturalEarth/MDG_Rivers.gpkg')
crs(rv) <- crs_standard
gaz[[3]] <- rv
names(gaz)[[3]] <- 'Rivers'

# protected areas
pa <- vect('naturalEarth/MDG_ProtAreas.gpkg')
crs(pa) <- crs_standard
gaz[[4]] <- pa
names(gaz)[[4]] <- 'Protected Areas'

#### Load Occurrence data ####
data <- read.csv("Data/Frogsdescribedrev.csv", sep = ';')
data <- data[,c(1,3,2,4)]
colnames(data)[1] <- 'species'
colnames(data)[2] <- 'decimalLongitude'
colnames(data)[3] <- 'decimalLatitude'

data0 <- data %>% filter(Genetics == 'NO') %>% select(-4)

occurrences_sf <- vect(data0, 
                       geom = c("decimalLongitude", "decimalLatitude"), 
                   crs = crs_standard)

# samp bias analysis ####
out <- calculate_bias(data0, buffer = 0,
                      terrestrial = TRUE,
                      res = 0.1, gaz = gaz)

dir_e = 'sampBiasDesc'
create_and_set_directory(dir_e)

summary(out)

capture.output(
  summary(out),
  file = "samp_bias_summary_descNoGen_01_new.txt"
)
## boxplot #####
# Prepare data for ggplot
weights_long <- pivot_longer(out$bias_estimate,
                                cols = starts_with("w_"),
                                names_to = "bias_factor",
                                values_to = "weight")
# Clean up factor names (remove "w_" prefix)
weights_long$bias_factor <- gsub("^w_", "", weights_long$bias_factor)

weights_long$bias_factor <- gsub("\\.", " ", weights_long$bias_factor)

# Color scheme similar to sampBias
sampbias_colors <- c("#2171B5", "#6BAED6","#FFD700","#EE7600",
                     "#8B2252", "#49006A")

plot_out <- ggplot(weights_long, aes(x = reorder(bias_factor, weight, median),
                                     y = weight)) +
  geom_boxplot(fill = sampbias_colors[1:length(unique(weights_long$bias_factor))],
               alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.8, color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  coord_flip() +  # Flip coordinates like sampBias
  labs(title = "Bias Weights",
       x = NULL,
       y = "Weight") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 11, face = "bold"))

ggsave(
  filename = "descNogen_01.png",  # Output filename
  plot = plot_out,                   # ggplot object
  device = "png",                   # Format
  width = 8,                        # Width (inches)
  height = 5,                       # Height (inches)
  units = "in",                     # Units for width/height ("in", "cm", "mm")
  dpi = 600,                        # High resolution
  bg = "white"                      # Background color
)

## Distance decay curves #####
# 1. Extract mean parameters from the MCMC output
mean_params <- colMeans(out$bias_estimate)
mean_q <- mean_params["q"]
mean_weights <- mean_params[grepl("^w_", names(mean_params))]

# 2. Create distance sequence
max_dist <- max(terra::values(out$distance_rasters), na.rm = TRUE)
dist_seq <- seq(0, max_dist, length.out = 1000)
rescaled_dist <- dist_seq / out$summa$rescale_distances

# 3. Calculate sampling rates for all factors
curve_data <- data.frame(distance = dist_seq)

for (factor_name in names(mean_weights)) {
  w <- mean_weights[factor_name]
  curve_data[[factor_name]] <- mean_q * exp(-w * rescaled_dist)
}

# 4. Convert to long format for ggplot
curve_long <- curve_data %>%
                  pivot_longer(
                    cols = -distance,
                    names_to = "bias_factor",
                    values_to = "sampling_rate"
                  ) %>%
                  mutate(
                    # Clean up factor names for plotting
                    bias_factor_clean = gsub("^w_", "", bias_factor),
                    bias_factor_clean = gsub("\\.", " ", bias_factor_clean)
                  )

# 5. Create the combined plot
distance_plot <- ggplot(curve_long, 
                        aes(x = distance, 
                            y = sampling_rate, 
                            color = bias_factor_clean)) +
                        geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_color_manual(values = sampbias_colors[1:length(mean_weights)]) +
                  labs(title = "",
                    x = "Distance to bias (km)",
                    y = "Sampling Rate",
                    color = "Bias Factor") +
                  theme_minimal() +
                  theme(legend.position = "bottom",
                    legend.text = element_text(size = 10),
                    panel.grid.major = element_line(color = "grey90"),
                    panel.grid.minor = element_line(color = "grey95")) +
                  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file = "descNogen_distance_bias_plot01.png", plot = distance_plot,
       width = 8, height = 4, dpi = 600)

# Plot SampBias maps ####
proj <- project_bias(out)
saveRDS(proj,'proj_samp_bias_combined_01')
map_bias(proj)

raster_bias <- proj[[1:4]]
raster_bias <- mask(raster_bias, area)
raster_bias <- crop(raster_bias, area)
names(raster_bias[[1]]) <- 'Prot. Areas'
names(raster_bias[[2]]) <- 'Prot. Areas + Roads'
names(raster_bias[[3]]) <- 'Prot. Areas + Roads\n Pop. places'
names(raster_bias[[4]]) <- 'Prot. Areas + Roads\n Pop. places + Rivers'


# Plot map combined_SamplingRate_01 ####
ggplot() +
  geom_spatraster(data = raster_bias) +
  geom_sf(data = occurrences_sf, aes(geometry = geometry),
          fill = NA, color = "black", size = 0.1) +  #
  facet_wrap(~lyr, ncol = 2) + # Facet by layer
  scale_fill_viridis_c(option = "plasma", na.value = "transparent")+
  labs(title = "", fill = "Sampling rate") +
  theme_minimal()

ggsave("combined_SamplingRate_01.png", plot = last_plot(), device = "png",
       width = 8, height = 10, units = "in", dpi = 600)
# Now map combined_difftoMax ####
raster_bias1 <- proj[[5]]
raster_bias1 <- mask(raster_bias1, area)
raster_bias1 <- crop(raster_bias1, area)

ggplot() +
  geom_spatraster(data = raster_bias1) +
  geom_spatvector(data = pa, aes(geometry = geometry,), 
                  fill = NA, color = "grey50", linewidth = 0.5) + 
  geom_spatvector(data = roads, aes(geometry = geometry), 
                  fill = NA, color = "white", linewidth = 0.5,
                  linetype = "longdash") + 
  geom_sf(data = occurrences_sf, aes(geometry = geometry),
          fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent")+
  labs(title = " ", fill = "Diff. to max") +
  theme_minimal()

ggsave("combined_difftoMax_01_rev.svg", plot = last_plot(), 
       device = "svg",
       width = 7, height = 10, units = "in", dpi = 600)



