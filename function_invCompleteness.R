## Functions #### 

#1 Calculate inventory completeness using KnowBR ####
knowbr_function <- function(d = NULL, id = NULL){
  KnowBPolygon(data = d, shape = grid, admAreas = FALSE,  # Use predefined grid as personalized polygons  
               shapenames = id,  # unique "id" cell from grid
               # minLon = ext$xmin[[1]], maxLon = ext$xmax[[1]], 
               # minLat = ext$ymin[[1]], maxLat = ext$ymax[[1]], # study area extent
               jpg = TRUE, dec = ".")
}
# 2. Plot map of occurrences by Gen information ####
occ_gen_map <- function(data1, title){
  map_points <- ggplot() +
    geom_sf(data= study_area_pol, fill = 'lightgrey', color = "lightgrey", linewidth = 0.7) +
    geom_sf(data = grid, fill = "transparent", color = "black") +
    geom_sf(data = data1, color = 'purple') +
    # geom_sf(data = data2, color = 'orange') +
    coord_sf(crs = crs) +
    ggtitle(title) +
    theme_minimal() + 
    theme(plot.background = element_rect(fill = 'white', color = 'white'))
}
# 3. Plot completeness estimators for establishing threshold of well-surveyed cells ----
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
#. 4. Plot of inv. completeness values map
complt_plotMap <- function(comp_shp, title){
  ggplot() +
    geom_sf(data = study_area_pol, fill = 'lightgrey', color = 'darkgrey') +
    geom_sf(data = comp_shp, aes(fill = Completeness), color = "transparent") +
    scale_fill_viridis_c(limits = c(0, 100), option = "plasma", name = "", 
                         direction = -1) +
    theme_minimal() +
    labs(title = title)+
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "none", 
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
}

complt_plotMap2 <- function(comp_shp,estWS, title){
  ggplot() +
    geom_sf(data = study_area_pol, fill = 'lightgrey', color = 'darkgrey') +
    geom_sf(data = comp_shp, aes(fill = Completeness), color = "transparent") +
    geom_sf(data = estWS, fill = 'transparent', color = "black", linewidth = 1) +
    scale_fill_viridis_c(limits = c(0, 100), option = "plasma", name = "", 
                         direction = -1) +
    theme_minimal() +
    labs(title = title)+
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "none", 
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
}
# 5. Scenario comparison map by scale
comparison_map <- function(){
  ggplot() +
    geom_sf(data = study_area_pol, fill ='lightgrey', color = 'darkgrey') +
    geom_sf(data = est_sum2, aes(fill = combinations)) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = "transparent"),
          legend.position = "bottom", 
          plot.title = element_text(size = 16, face = "bold", hjust = 1.1))
}
