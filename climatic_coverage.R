wd <- 'C:/Users/Joaquin Hortal/Desktop/NICED_SCENIC/Albert-Ranas'
# wd <- 'C:/Users/MNCN-JHICA/Desktop/proyectoInveCov'
setwd(wd)

# Load study area ####
crs = 'EPSG:4326'
study_area_pol <- read_sf('shpfiles/studyArea4326_mdg&myt.gpkg')
study_area_pol_crs <- st_transform(study_area_pol, crs)
grid <- st_read('shpfiles/grid_01.gpkg')
grid_crs <- st_transform(grid, crs)

grid_crs <- st_join(grid_crs, study_area_pol_crs, join = st_intersects)
grid_crs <- grid_crs %>% filter(!is.na(NAME_0))
plot(grid_crs)


# Load occurrences dataset (3 column format: Species, Longitude, Latitude) ####
data0 <- fread('Data/.csv', sep = ';') #combinedFrogsrev #frogsDescriberev
data0 <- data0[Genetics == 'NO',]
data_points <- vect(data0, geom = c("Longitude", "Latitude"),
                    crs = crs)
# Load Climate data #####
climwd <- 'C:/Users/Joaquin Hortal/Desktop/gis_layers/CHELSA_10km'
# climwd <- 'C:/Users/MNCN-JHICA/Downloads/chelsa'
rlist <- list.files(climwd, pattern = "*.tif$")

setwd(climwd)
climRaster <- rast(rlist)
raster_mask <- mask(climRaster, study_area_pol)
raster_mask <- crop(raster_mask, study_area_pol)
climRaster_res <- raster_mask

# IF aggreg is nedded run this section - set fact!
climRaster_res <- aggregate(climRaster_res, fact = 3, fun = "mean")
plot(climRaster_res[[1]])
res(climRaster_res)

setwd(wd)
### Load standardization, Schoener D, kruskal Wallis functions ####
std <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}
# Schoener's D: quantifies the overlap between the location of well-sampled
# sites and cells with most frequent conditions
# The Schoener's D index varies from zero (total lack of congruence) to one
# (total congruence) indicating that the location of well-sampled
# sites coincide with climate conditions that are frequently found in the study area
SchoenersD <- function(x, y) {
  sub_values <- abs(x - y)
  D <- 1 - (sum(sub_values, na.rm = TRUE) / 2)
  return(D)
}
# Function to extract and save test statistics
save_kruskal_csv <- function(formula, filename = "kruskal_stats.txt"){
  result <- kruskal.test(formula)
  # Create data frame with results
  results_df <- data.frame(
    Test = deparse(formula),
    Chi_squared = result$statistic,
    DF = result$parameter,
    P_value = result$p.value,
    Method = result$method,
    StringsAsFactors = FALSE
  )
  
  # Save to CSV
  write.csv(results_df, filename, row.names = FALSE)
  message(paste("Results saved to:", filename))
  return(results_df)
}
# PCA ####
# First we can reduce the variables to fewer variables using a PCA.
# Here we standardize and prepare the data.
v <- as.data.frame(values(climRaster_res)) # get env values from rasters
# str(v)
v2 <- apply(v, 2, std) # apply standardization function to dataframe of climate values
rem <- apply(is.na(v2), 1, any)
PCAdata <- as.data.frame(v2[!rem, ])
# Run the PCA
mat <- matrix(runif(nrow(PCAdata) * ncol(PCAdata), 0.00001, 0.00009),
              ncol = ncol(PCAdata)) # add a very small randomness to avoid singularity
PCAdata2 <- PCAdata + mat
myPCA <- principal(PCAdata2,
                   nfactors = 2,
                   rotate = "varimax",
                   scores = T)
# prop.table(myPCA$values)
pc1_clim <<- myPCA$Vaccounted["Proportion Var", "RC1"]
pc2_clim <<- myPCA$Vaccounted["Proportion Var", "RC2"]

# Values of PCA of our study area ####
# Instead of assuming the absolute values of all bioclimatic variables,
# the next map assumes values of the linear combinations between these variables (PCA scores).
v3 <- v2[, 1:2] # create a vector with the same length as v2 but 2 columns
v3[!rem, ] <- myPCA$scores # insert PCA_scores (2axis-2columns) to the new vector
climate_PCA <- subset(climRaster_res, 1:2) # extract 2 raster layer as a layerbase to insert our pca values
values(climate_PCA) <- v3 # insert pca values of v3 into raster
names(climate_PCA) <- c("PC1", "PC2") # rename

# Environmental space ####
# The next step is to create the environmental space using the two PCA scores.
# Creates a Cartesian plan with PC1 and PC2 scores
v4 <- values(climate_PCA) # get env values from PCA
# Transform the two vars we want into the env space,
# by taking the min and max scores (PCA score values) for each PCA axis
# and create a raster object.
xmin <- min(v4[, 1], na.rm = TRUE)
xmax <- max(v4[, 1], na.rm = TRUE)
ymin <- min(v4[, 2], na.rm = TRUE)
ymax <- max(v4[, 2], na.rm = TRUE)
# This function creates the cartesian plan comprising the min and max PCA scores
env_space_res = 0.2
env_space <- rast(xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax,
                  res = env_space_res) # SET HERE size of bins
values(env_space) <- 0
env_space <- env_space
env_space_area <- env_space # duplicate this object for the next step

# Insert our PCA values into this env_space
# Extract PC1 and PC2, convert it in "classes of values" to plot in the map
env_space_v <- terra::extract(env_space,
                              y = na.omit(v4),
                              cells = TRUE)[, 1]
n_env_space_v <- table(env_space_v) # counts the frequency of "climates" (PC scores)

values(env_space_area)[as.numeric(names(n_env_space_v))] <- n_env_space_v
area_values <- values(env_space_area)
area_values[area_values == 0] <- NA
nCellTotal <- length(area_values[!is.na(area_values)])
values(env_space_area) <- area_values

# Env. Space of all occurrences ####
stack <- env_space_area
values_All <- cells(climRaster_res,
                    data_points)[, 2]
coords_All <- v4[values_All, ]
cell_All <- terra::extract(env_space,
                           coords_All,
                           cells = TRUE)[, 1]
n_All <- table(cell_All)
env_space_All <- env_space
values(env_space_All)[as.numeric(names(n_All))] <- n_All
env_space_All[env_space_All == 0] <- NA

stack <- c(stack, env_space_All) # Join the new raster from order
names(stack[[1]]) <- 'Study area'
names(stack[[2]]) <- 'Species Occurrences'
# Well surveyed cells #########
# Env. Space of all occurrences of order
WS_cent <- read.csv(paste0(wd, '/outputs_knowBR/WS_centroids_comb_01_nogen.csv'), sep=";")

WS_centroids <- vect(WS_cent,
                      geom = c("lon", "lat"), crs = crs)
values_WS <- cells(climRaster_res, WS_centroids)[, 2]
coords_WS <- v4[values_WS, ]
cell_WS <- terra::extract(env_space,
                          coords_WS,
                          cells = TRUE)[, 1]
n_WS <- table(cell_WS)
env_space_WS <- env_space
values(env_space_WS)[as.numeric(names(n_WS))] <- n_WS
env_space_WS[env_space_WS == 0] <- NA
all_WS <- unique(cell_WS)
stack <- c(stack, env_space_WS) # Join the new raster from order
names(stack[[3]]) <- 'Well Surveyed cells'
stackF <<- stack
plot(stack)
# Schoener's D ####
# Transform the abundance of each cell into probabilities.
# Relative frequency of climate type for all the study area
area_values <- area_values / sum(area_values, na.rm = TRUE)
WS_values <- values(env_space_All)
# Relative frequency of climate type for well-sampled cells
WS_values <- WS_values/sum(WS_values, na.rm = TRUE)

D <- SchoenersD(area_values, WS_values)
SchoenerD <<- D
print(paste("Climate overlap between well-sampled cells and the study area,",
            "given by the observed Schoener's D equals = ",
            round(D,3), "%"))

# We create a null model to test if D values is different from a
# random distribution of D values calculated from randomly sampling occurrence
# records.
set.seed(0)
replications <- 1000 # Choose the number of replications
D_rnd <- numeric(replications)
for(i in 1:replications){
  rnd <- sample(env_space_v, nrow(data0), replace = TRUE)
  n_rnd <- table(rnd)
  env_space_rnd <- env_space
  values(env_space_rnd)[as.numeric(names(n_rnd))] <- n_rnd
  rnd_values <- values(env_space_rnd)
  rnd_values <- rnd_values/sum(rnd_values, na.rm = TRUE)
  D_rnd[i] <- SchoenersD(area_values, rnd_values)
}
# p-value from previous analysis
# If p <0.05, it means that the location of well-sampled sites does not
# coincide with areas with climate conditions frequently found in your study area
p <- (sum(D > D_rnd) + 1) / (length(D_rnd) + 1) # Unicaudal test
pvalueD <<- p
print(paste("p value equals = ", round(p, 3)))

# Kruskal-Wallis and kolmogorov Smirnov tests ####
setwd(paste0(wd,'/climatic_coverage/comb_01_nogen'))
for (pcaAxis in 1:2){
  # X axis is a probability density
  # Kruskal-Wallis verifies whether 1) the distribution of well-sampled sites
  # is an unbiased subset of the entire climate conditions of the Atlantic forest.
  # If this is so, p > 0.05
  x_axis <- c(myPCA$scores[, pcaAxis], coords_WS[, pcaAxis])
  g_axis <- as.factor(c(rep("area", length(myPCA$scores[, pcaAxis])),
                        rep("WS", length(coords_WS[, pcaAxis]))))
  stats_df <- save_kruskal_csv(x_axis ~ g_axis,
                               paste('Axis', pcaAxis, "kruskalWallis_stats.txt"))
  # Kolmogorov smirnov test###
  writeLines(capture.output(
    ks.test(myPCA$scores[, pcaAxis], coords_WS[, pcaAxis])),
    paste('Axis', pcaAxis, "ks_two_sample.txt"))
  
}

### Rarity analyses ########
# We can check how many environmental space has been sampled
# and how does these cells look like.
  sampled <- length(n_WS)/length(n_env_space_v)
  percen <- round(sampled, 2)*100
  prin <- paste0(percen, "% of our study area climate types covered by well-sampled cells")
  print(prin)
  
  surface <- WS_values
  surface[is.na(area_values) | area_values == 0] <- NA
  # Is this env. space sampled corresponding to rare climates?
  # First, we make values vary from 0 to 1 according to their rarity:
  # Values close to 0, are very common, values close to 1 very rare
  mini <- min(area_values, na.rm = TRUE) # less frequent value
  # rarity index, also called Min-Max scalling
  area_values01 <- abs(1 - (area_values - mini) /
                         (max(area_values, na.rm = TRUE) - mini))
  # see http://rasbt.github.io/mlxtend/user_guide/preprocessing/minmax_scaling/
  
  # Kruskal-Wallis test to see if the distribution of rarities for the sampled
  # occurrences differs from the distribution observed in the entire area
  x_kw <- c(area_values01, area_values01[surface > 0])
  g_kw <- as.factor(c(rep("area", length(area_values01)),
                      rep("ws", length(area_values01[surface > 0]))))

  save_kruskal_csv(x_kw ~ g_kw, "rarity_kruskal_Wallis_stats.txt")
  # Kolmogorov smirnov test
  area_values03 <- na.omit(area_values01)
  area_values04 <- na.omit(area_values01[surface > 0])
  writeLines(capture.output(
    ks.test(area_values03, area_values04)),
   'rarity_ks_two_sample.txt')
  
  # Finally MAP the climatic rarity
  rarity_env <- env_space
  values(rarity_env) <- area_values01
  rarity_Percell <- terra::extract(rarity_env,
                                   v4, cells = TRUE)[, 2]

  rarity_map <- climRaster_res[[1]]
  values(rarity_map) <- rarity_Percell
  plot(rarity_map)
  
  stackF <- c(stackF, rarity_env) # Join the new raster from order
  names(stackF[[4]]) <- 'Rarity index'
plot(stackF)  

# Plot environmental spaces ####
simple_labels <- function(x) {format(round(x, 1), nsmall = 1)} # Rounds to 1 decimal place
my_window <- ext(stack) # Env. space limits

### Env space comparison (study area vs occurrences vs WS cells)
env_space_area_df <- as.data.frame(stackF[[1]], xy = TRUE)
env_space_All_df <- as.data.frame(stackF[[2]], xy = TRUE)
env_space_WS_df <- as.data.frame(stackF[[3]], xy = TRUE)

env_space_area_df <- env_space_area_df[env_space_area_df$`Study area` != 0, ]
env_space_All_df <- env_space_All_df[env_space_All_df$`Species Occurrences` != 0, ]
env_space_WS_df <- env_space_WS_df[env_space_WS_df$`Well Surveyed cells` != 0, ]
names(env_space_area_df)[3] <- "freq"

env_plot2 <- ggplot() +
  geom_raster(data = env_space_area_df, aes(x = x, y = y),
              fill = '#CDC0B0',
              alpha = 0.5) +
  # geom_spatvector(data = pol_clim_occ, 
  #                 fill = 'transparent', color = 'black') +
  geom_raster(data = env_space_WS_df, aes(x = x, y = y),
              fill = '#495970') +
  ggnewscale::new_scale_fill() +
  #PC1 density (marginal)
  geom_xsidedensity(
    data = as.data.frame(myPCA$scores),
    aes(x = myPCA$scores[,1], y = after_stat(scaled),
        fill = "Overall climate conditions"),
    alpha = 0.5, linewidth = 0, linewidth = 0.3,
    colour = "black",
    linetype = "solid"
  ) +
  geom_xsidedensity(
    data = coords_WS,
    aes(x = coords_WS[,1], y = after_stat(scaled),
        fill = "Well-surveyed cells climate conditions"),
    alpha = 0.3, linewidth = 0, linewidth = 0.3,
    colour = "black",
    linetype = "dashed"
  ) +
  #PC2 density (marginal)
  geom_ysidedensity(
    data = as.data.frame(myPCA$scores),
    aes(y = myPCA$scores[,2], x = after_stat(scaled),
        fill = "Overall climate conditions"),
    alpha = 0.5, linewidth = 0.3,
    colour = "black",
    linetype = "solid"
  ) +
  geom_ysidedensity(
    data = as.data.frame(coords_WS),
    aes(y = coords_WS[,2], x = after_stat(scaled),
        fill = "Well-surveyed cells climate conditions"),
    alpha = 0.3, linewidth = 0.3,  
    colour = "black",
    linetype = "dashed"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Overall climate conditions" = "#CDC0B0",
      "Well-surveyed cells climate conditions" = "#495970"
    ),
    guide = 'none'
  ) +
  #Reference lines
  geom_xsidehline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_ysidevline(xintercept = 0, color = "black", linewidth = 0.5) +
  #Marginal axis scales
  scale_xsidey_continuous(position = "right", n.breaks = 2, expand = c(0, 0),
                          labels = function(x) ifelse(x == 0, "0", x)) +
  scale_ysidex_continuous(position = "top", n.breaks = 2, expand = c(0, 0),
                          labels = function(x) ifelse(x == 0, "", x)) +
  theme_classic() +
  theme(
    ggside.panel.scale.x = 0.2,
    ggside.panel.scale.y = 0.2,
    plot.margin = margin(t = 10, b = 10, r = 20, l = 10, unit = "pt"),
    ggside.axis.line = element_line(linewidth = 0.3),
    ggside.axis.text.y.right = element_text(angle = c(325, 0), hjust = -0.15, vjust = -0.2),
    ggside.axis.text.x.top = element_text(
      angle = -90, hjust = 0, vjust = 0),
    ggside.axis.title.y.right = element_blank(),
    ggside.axis.ticks = element_line(linewidth = 0.3),
    ggside.axis.text.x = element_blank(),
    ggside.axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x  = element_blank(),
    axis.text = element_text(size = 10, face = 'bold'),
    legend.position = "none",
    legend.box = "vertical",
    legend.justification = "center",
    legend.title.align = 0.5,
    legend.title = element_text(size = 14),
    legend.margin = margin(t = 0, b = 0),
    legend.box.spacing = unit(0.8, 'cm'),
    legend.spacing.y = unit(0.3, 'cm'),
    legend.spacing.x = unit(0.8, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.text = element_text(size = 12,
                               margin = margin(l = 3, r = 2, t = 3)))
#Input marginal subtitles
env_plot2 <- gridExtra::grid.arrange(
  ggplotGrob(env_plot2),
  left = grid::textGrob(paste0("Climate PC2 (", round(pc2_clim * 100, 1), "%)"),
                        rot = 90, vjust = 1, hjust = 0.5,
                        gp = grid::gpar(fontsize = 14, fontface = 'bold')),
  bottom = grid::textGrob(paste0("Climate PC1 (", round(pc1_clim * 100, 1), "%)"),
                          vjust = 0, hjust = 0.7,
                          gp = grid::gpar(fontsize = 14, fontface = 'bold')))

# Save using ggsave
ggsave('envSpacePlots_freq_comb01.png', env_plot2, width = 6, 
       height = 6, dpi = 600)

## Plot rarity ####
# Rarity map plot
rarity_palette <- colorRampPalette(c( "#8B8B7A","#CDCDB4", "#FFFFE0",
                                        "#FFC0CB",  "#CD6889", "#8B475D"))
rarity_gradient_colors <- rarity_palette(100)
create_density_plot <- function(){
  plot(density_data,
       main = "",
       xlab = "Climate rarity index",
       ylab = 'Frequency',
       ylim = c(0, max(density_data$y) * 1.1),
       font = 2, font.lab = 2,
       cex.lab = 1.6, cex.axis = 1.6,
       type = "n") 
  # grad under curve
  x <- density_data$x
  y <- density_data$y
  for(i in 1:(length(x)-1)) {
    polygon(x = c(x[i], x[i+1], x[i+1], x[i]),
            y = c(0, 0, y[i+1], y[i]),
            col = rarity_gradient_colors[floor(i/length(x)*100)],
            border = NA)
  }
  
  # Añadir la línea superior de celdas bien muestradas
  lines(density(na.omit(area_values01[surface > 0])), col = "black", lwd = 2, lty = 2)
}

envspace_rarity_plot <- ggplot()  +
    geom_spatraster(data = rarity_env, aes(fill = after_stat(value))) +
    scale_fill_gradientn(colours = rarity_gradient_colors,
                         na.value = "transparent", name = '') +
    geom_tile(data = env_space_WS_df, aes(x = x, y = y),
              fill = NA, color = "black", linewidth = 0.8) +
    scale_y_continuous(labels = simple_labels) +
    scale_x_continuous(labels = simple_labels) +
    xlab('PC1') + ylab('PC2') +
    theme_minimal()
ggsave('comb01_env_space_rarity.png', envspace_rarity_plot, 
       height = 6, width = 6, dpi = 600)

rarPlot <- ggplot() +
    geom_spatraster(data = rarity_map, aes(fill = after_stat(value))) +
    scale_fill_gradientn(colours = rarity_gradient_colors,
                         na.value = "transparent", name ='') +

    geom_spatvector(data = WS_centroids, color = "black", size = 2) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
ggsave('comb01_map_rarity.png', rarPlot, 
       height = 6, width = 6, dpi = 600)

density_data <- density(na.omit(area_values01))

png("comb01_rarity_freq.png",
    width = 10,    # 10 pulgadas
    height = 5,    # 8 pulgadas  
    units = "in",       # Alto en píxeles
    res = 600)     # Tamaño de fuente base

create_density_plot()

dev.off()

