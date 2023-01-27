###################################################################################################
###############################  ------ WATER SOURCE PROFILE ---- #################################
###################################################################################################


# \ 
# 
# This code analyzes the sensitivity
#   
#   
#   \


## ENVIRONMENT SETTINGS =============================================================


## DPE/Petter to provide LUT between HARC and water sources
## match harc_annual_impact_100_df  to match NSW_water_sources



# STEP 1 :: read data ----


## Each file is a water source, with a code for the area.
## What are those codes? Check the differences here
harc_annual_impact_100_df <- read_csv(paste0(hydro_out, 'harc_annual_impact_100_Dams_flow_metrics.csv'))
HARC_Percentage_Change    <- read_excel_allsheets('./data/enviro/hydro/HARC_Percentage_Change_Flow.xlsx') %>% 
  .$Sheet3 %>% arrange(Harv_Limit )


harc_list   <- unique(harc_annual_impact_100_df$Water_source) 
harc_string <- paste0(harc_list, collapse = '|') 


# load('HARC_raster_feature_data_update.RData')


## Check enviro rasters
plot_rasters <- stack(water.source.grids.5km[['Max_temp_warm_month']],
                      water.source.grids.5km[['Mean_diurnal_range']],
                      water.source.grids.5km[['Min_temp_cold_month']],
                      water.source.grids.5km[['Temp_annual_range']])

ckey <- list(labels=list(cex=2))
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)

png(paste0(hydro_out, 'EG_rasters.png'), 
    16, 10, units = 'in', res = 800)                                                       

levelplot(plot_rasters,
          col.regions = terrain.colors(10),
          layout = c(2, 2),
          colorkey=ckey)

dev.off()



# STEP 2 :: Calculate zonal stats ----


## Lists of unique categories in each feature layer
land_uses     <- unique(NSW_land_use$Secondary)
water_sources <- unique(NSW_unreg_coastal_water_sources_aggregated$UNREGULATED_WATER_SOURCE) %>% 
  sort() %>% .[!. %like% harc_string]




## This needs to be linked to the HARC models


## Calculate areas
hectare_conversion <- 10000
km_conversion      <- 1000000


## Calculate Water source areas
## Read in the Project feature data here from the inputs geo-database
NSW_unreg_coastal_water_sources_aggregated <- 
  
  st_read(dsn = paste0(data_dir, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_unreg_coastal_water_sources_aggregated') %>% 
  
  ## Calculate the areas
  dplyr::mutate(Hectares  = st_area(geom)/hectare_conversion,
                Hectares  = drop_units(Hectares),
                Sq_km     = st_area(geom)/km_conversion,
                Sq_km     = drop_units(Sq_km),
                Perimeter = st_perimeter(geom),
                Perimeter = as.numeric(Perimeter),
                Shape     = Perimeter/sqrt(Hectares)) 


## Update this
NSW_Stream_Order   <- st_read(dsn   = paste0(DPEW_spatial_data, 
                                             "CHR.gdb"),
                              layer = 'Strahler_Stream_Order') %>% 
  st_transform(., st_crs(8058))


## Calculate the stream density per water source
## Join this on after the zonal stats
NSW_water_sources_stream_density <- 
  
  st_intersection(st_make_valid(NSW_unreg_coastal_water_sources_aggregated), 
                  st_make_valid(NSW_Stream_Order)) %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, 
                Hectares, Sq_km, Perimeter, Shape, STRAHLER, SHAPE_Length) %>% 
  
  group_by(UNREGULATED_WATER_SOURCE, W_Source_ID) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% as.data.frame() %>% 
  dplyr::select(UNREGULATE, SHAPE_Length) 





png(paste0(hydro_out, 'NSW_Rasters_plot.png'),
    16, 10, units = 'in', res = 600)

par(mfrow=c(2, 2), mar=c(1.2,1.2,1.2,1.2) + 0.5)

plot(water.source.grids.5km[['Annual_precip']], 
     col=rampGreens(), axes = FALSE, main = "Rainfall", legend.mar = 6, box = FALSE,
     legend.width = 3,
     # legend.only=TRUE, 
     legend.args=list(text='', side=5, font=3, line=2.5, cex=1.5),
     cex.lab=4, cex.axis=3, cex.main=2.5)

plot(water.source.grids.5km[['Aspect']], col=rampGreens(), legend.width = 3,
     axes = FALSE, main = "Aspect", legend.mar = 6, box = FALSE,
     cex.lab=4, cex.axis=3, cex.main=2.5)

plot(water.source.grids.5km[['SND']], col=rampGreens(), legend.width = 3,
     axes = FALSE, main = "Sand", legend.mar = 6, box = FALSE,
     cex.lab=4, cex.axis=3, cex.main=2.5)

plot(water.source.grids.5km[['Forest_Cov']], col=rampGreens(), legend.width = 3,
     axes = FALSE, main = "Forest Cover", legend.mar = 6, box = FALSE,
     cex.lab=4, cex.axis=3, cex.main=2.5)

dev.off()



harc_list


## Areas are similar to the DPI calcs.
## Perimter and shape are not accurate
Test_hectares <- NSW_water_sources %>% 
  filter(UNREGULATE == 'Candelo Creek Water Source') %>% 
  .$Hectares %>% sum() 

Test_perimeter <- NSW_water_sources %>% 
  filter(UNREGULATE == 'Candelo Creek Water Source') %>% 
  .$Perimeter %>% sum() 

Test_shape    <- NSW_water_sources %>% 
  filter(UNREGULATE == 'Candelo Creek Water Source') %>% 
  .$Shape %>% sum() 


## Create list of variables we want to use
water_source_vars <- c('LUE',
                       'rain_ann_5km', 'pet_ann_5km', 'Annual_precip',
                       'AWC',  'ASC', 'TWI', 'SLT', 'SND', 'BDW', 'CLY', 'ECE',
                       'Topo', 'DER', 'DES', 'Elevation', 'Slope', 'Aspect',
                       'Forest_Cov')


water_source_cols <- c('Water_source',     'UNREGULATE', 'HARC',  'MAJOR_CATC', 'NSW_ACT', 
                       'WATER_SHAR',       'WSP_ID',      
                       'Hectares',         'Sq_km',      'Perimeter', 'Shape')


harc_list     %in% water_sources %>% table()
water_sources %in% harc_list %>% table()


if(calc_zonal) {
  
  
  ## HARC zonal stats ----
  harc_sources_zonal_stats_sf <- harc_list %>%
    
    ## Pipe the list into lapply
    #source = harc_list[3]
    lapply(function(source) {
      
      message('calculate zonal stats for ', source)
      
      ## subset the source
      source_data <- NSW_water_sources %>% 
        dplyr::filter(., grepl(source, UNREGULATE))
      
      if(nrow(source_data) >= 1) {
        
        ## Extract the raster values for that Water_source
        zonal_median <- terra::extract(water.source.grids.5km, 
                                       source_data, 
                                       fun   = median, 
                                       na.rm = TRUE) %>% as_tibble() %>% 
          dplyr::select(all_of(water_source_vars))
        names(zonal_median) <- paste0('Med_', names(zonal_median))
        
        zonal_max <- terra::extract(water.source.grids.5km, 
                                    source_data, 
                                    fun   = max, 
                                    na.rm = TRUE) %>% as_tibble() %>% 
          dplyr::select(all_of(water_source_vars))
        names(zonal_max) <- paste0('Max_', names(zonal_max))
        
        zonal_data <- zonal_median %>% 
          bind_cols(., zonal_max)  
        
        ## Bind the data together
        source_data_zonal <- source_data %>% 
          
          bind_cols(., zonal_data) %>% 
          
          mutate(Water_source = source) %>% 
          mutate(HARC = 'HARC') %>%
          dplyr::select(one_of(water_source_cols), names(zonal_data)) %>% 
          
          ## 
          group_by(Water_source, MAJOR_CATC, NSW_ACT, WATER_SHAR, UNREGULATE, HARC) %>% 
          summarise_if(is.numeric, mean, na.rm = TRUE)
        
      } else {
        message('Check source data ', source, ' skip')
      }
      
    }) %>% bind_rows()
  
  
  
  
  
  ## All zonal stats ----
  all_sources_zonal_stats_sf <- water_sources %>%
    
    ## Pipe the list into lapply
    #source = harc_list[3]
    lapply(function(source) {
      
      message('calculate zonal stats for ', source)
      
      ## subset the source
      source_data <- NSW_water_sources %>% 
        dplyr::filter(., grepl(source, UNREGULATE))
      
      if(nrow(source_data) >= 1) {
        
        ## Extract the raster values for that Water_source
        zonal_median <- terra::extract(water.source.grids.5km, 
                                       source_data, 
                                       fun   = median, 
                                       na.rm = TRUE) %>% as_tibble() %>% 
          dplyr::select(all_of(water_source_vars))
        names(zonal_median) <- paste0('Med_', names(zonal_median))
        
        zonal_max <- terra::extract(water.source.grids.5km, 
                                    source_data, 
                                    fun   = max, 
                                    na.rm = TRUE) %>% as_tibble() %>% 
          dplyr::select(all_of(water_source_vars))
        names(zonal_max) <- paste0('Max_', names(zonal_max))
        
        zonal_data <- zonal_median %>% 
          bind_cols(., zonal_max) 
        
        ## Bind the data together
        source_data_zonal <- source_data %>% 
          
          bind_cols(., zonal_data) %>% 
          
          mutate(Water_source = source) %>% 
          mutate(HARC = 'OTHER') %>%
          dplyr::select(one_of(water_source_cols), names(zonal_data)) %>% 
          
          ## 
          group_by(Water_source, MAJOR_CATC, NSW_ACT, WATER_SHAR, UNREGULATE, HARC) %>% 
          summarise_if(is.numeric, mean, na.rm = TRUE)
        
      } else {
        message('Check source data ', source, ' skip')
      } 
      
    }) %>% bind_rows()
  
  
  save.image('HARC_zonal_data_update.RData')
  
} else {
  
  message('read zonal data from file')
  load("./HARC_zonal_data_update.RData")
  
}





# STEP 3 :: Consolidate data ----


## Combine the spatial data
all_sources_zonal_stats_sf_join <- all_sources_zonal_stats_sf %>% 
  
  bind_rows(., harc_sources_zonal_stats_sf) %>% 
  
  left_join(., NSW_water_sources_stream_density, by = "UNREGULATE") %>% 
  mutate(Stream_density     = Stream_Length/Hectares,
         Log_Stream_density = log(Stream_density))



## Create a map of the output
forest_map_eg <- tm_shape(all_sources_zonal_stats_sf_join) +
  
  tm_polygons(col          = "Med_Forest_Cov", 
              palette      = "Greens", 
              style        = "jenks",
              # legend.hist  = TRUE, 
              n = 5) +
  
  tm_layout(legend.outside = TRUE,
            legend.title.size = 6,
            legend.text.size = 1.0,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            # legend.digits = 5,
            legend.bg.alpha = 1,
            frame = F, fontface = 2) 


index_map_eg <- tm_shape(all_sources_zonal_stats_sf_join) +
  
  tm_polygons(col          = "Med_TWI", 
              palette      = "YlOrRd", 
              style        = "jenks",
              # legend.hist  = TRUE, 
              n = 4) +
  
  tm_layout(legend.outside = TRUE,
            legend.title.size = 6,
            legend.text.size = 1.0,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            # legend.digits = 5,
            legend.bg.alpha = 1,
            frame = F, fontface = 2) 


slope_map_eg <- tm_shape(all_sources_zonal_stats_sf_join) +
  
  tm_polygons(col          = "Med_Slope", 
              palette      = "Greys", 
              style        = "jenks",
              # legend.hist  = TRUE, 
              n = 5) +
  
  tm_layout(legend.outside = TRUE,
            legend.title.size = 6,
            legend.text.size = 1.0,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            # legend.digits = 5,
            legend.bg.alpha = 1,
            frame = F, fontface = 2) 


stream_map_eg <- tm_shape(all_sources_zonal_stats_sf_join) +
  
  tm_polygons(col          = "Log_Stream_density", 
              palette      = "Blues", 
              style        = "jenks",
              # legend.hist  = TRUE, 
              n = 5) +
  
  tm_layout(legend.outside = TRUE,
            legend.title.size = 6,
            legend.text.size = 1.0,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            # legend.digits = 5,
            legend.bg.alpha = 1,
            frame = F, fontface = 2) 


# tmap_arrange(index_map_eg,  
#              forest_map_eg, 
#              slope_map_eg,
#              stream_map_eg,
#              widths = c(.25, .75))


png(paste0(hydro_out, 'forest_eg.png'),
    10, 16, units = 'in', res = 800)
forest_map_eg
dev.off()

png(paste0(hydro_out, 'Index_eg.png'),
    10, 16, units = 'in', res = 800)
index_map_eg
dev.off()

png(paste0(hydro_out, 'Slope_eg.png'),
    10, 16, units = 'in', res = 800)
slope_map_eg
dev.off()

png(paste0(hydro_out, 'Stream_eg.png'),
    10, 16, units = 'in', res = 800)
stream_map_eg
dev.off()


png(paste0(hydro_out, 'Raster_rain.png'),
    10, 16, units = 'in', res = 800)

plot(water.source.grids.5km[['Annual_precip']], 
     col=rampGreens(), axes = FALSE, main = "Rainfall", legend.mar = 6, box = FALSE,
     legend.width = 3,
     # legend.only=TRUE, 
     legend.args=list(text='', side=5, font=3, line=2.5, cex=1.5),
     cex.lab=4, cex.axis=3, cex.main=2.5)

dev.off()png(paste0(hydro_out, 'Raster_rain.png'),
             10, 16, units = 'in', res = 800)




## Create a simple table of the output
harc_sources_zonal_stats_table <- harc_sources_zonal_stats_sf %>%
  
  as_tibble() %>% 
  left_join(., NSW_water_sources_stream_density, by = "UNREGULATE") %>% 
  mutate(Stream_density     = Stream_Length/Hectares,
         Log_Stream_density = log(Stream_density))



all_sources_zonal_stats_table <- all_sources_zonal_stats_sf %>%
  
  as_tibble() %>% 
  mutate(HARC = 'OTHER') %>%
  left_join(., NSW_water_sources_stream_density, by = "UNREGULATE") %>% 
  mutate(Stream_density     = Stream_Length/Hectares,
         Log_Stream_density = log(Stream_density))




## Combined table
combined_sources_zonal_stats_table <- bind_rows(all_sources_zonal_stats_table,  
                                                harc_sources_zonal_stats_table) %>% 
  na.omit() %>% as.data.frame()

table(combined_sources_zonal_stats_table$HARC)



## Get the flow metrics
harc_impact_100_10_flow_metrics <- 
  
  harc_annual_impact_100_df  %>% 
  dplyr::filter(Impact_Model == 'Q_impact_100_10_2') 


harc_impact_100_20_flow_metrics <- 
  
  harc_annual_impact_100_df  %>% 
  dplyr::filter(Impact_Model == 'Q_impact_100_20_2') 


harc_impact_100_30_flow_metrics <- 
  
  harc_annual_impact_100_df  %>% 
  dplyr::filter(Impact_Model == 'Q_impact_100_30_2') 


harc_impact_100_50_flow_metrics <- 
  
  harc_annual_impact_100_df  %>% 
  dplyr::filter(Impact_Model == 'Q_impact_100_50_2') 


## Join the HARC models to the LUT
Combined_harc_source_100_10_flow_measures_sf <- left_join(harc_sources_zonal_stats_table,
                                                          harc_impact_100_10_flow_metrics,
                                                          by = c('Water_source', 'HARC')) %>% 
  
  mutate(UNREGULATE = gsub('Water Source', '', UNREGULATE))


Combined_harc_source_100_20_flow_measures_sf <- left_join(harc_sources_zonal_stats_table,
                                                          harc_impact_100_20_flow_metrics,
                                                          by = c('Water_source', 'HARC')) %>% 
  
  mutate(UNREGULATE = gsub('Water Source', '', UNREGULATE))


Combined_harc_source_100_30_flow_measures_sf <- left_join(harc_sources_zonal_stats_table,
                                                          harc_impact_100_30_flow_metrics,
                                                          by = c('Water_source', 'HARC')) %>% 
  
  mutate(UNREGULATE = gsub('Water Source', '', UNREGULATE))


Combined_harc_source_100_50_flow_measures_sf <- left_join(harc_sources_zonal_stats_table,
                                                          harc_impact_100_50_flow_metrics,
                                                          by = c('Water_source', 'HARC')) %>% 
  
  mutate(UNREGULATE = gsub('Water Source', '', UNREGULATE))



## 
Combined_harc_source_100_10_flow_measures_df <- Combined_harc_source_100_10_flow_measures_sf %>% as_tibble()
Combined_harc_source_100_20_flow_measures_df <- Combined_harc_source_100_20_flow_measures_sf %>% as_tibble()
Combined_harc_source_100_30_flow_measures_df <- Combined_harc_source_100_30_flow_measures_sf %>% as_tibble()
Combined_harc_source_100_50_flow_measures_df <- Combined_harc_source_100_50_flow_measures_sf %>% as_tibble()





# STEP 4 :: Save data ----


## Save image here


## Save the data out. 
## Should save geo-packages, etc. eventually
st_write(all_sources_zonal_stats_sf_join, 
         paste0(hydro_out, 'all_sources_zonal_stats_sf_join.shp'),
         driver = "ESRI Shapefile") 


write_csv(Combined_harc_source_100_10_flow_measures_df, 
          paste0(hydro_out, 'Combined_harc_source_100_10_flow_measures_table.csv'))


write_csv(Combined_harc_source_100_20_flow_measures_df, 
          paste0(hydro_out, 'Combined_harc_source_100_20_flow_measures_table.csv'))


write_csv(Combined_harc_source_100_30_flow_measures_df, 
          paste0(hydro_out, 'Combined_harc_source_100_30_flow_measures_table.csv'))


write_csv(Combined_harc_source_100_50_flow_measures_df, 
          paste0(hydro_out, 'Combined_harc_source_100_50_flow_measures_table.csv'))


write_csv(all_sources_zonal_stats_table, 
          paste0(hydro_out, 'all_water_sources_zonal_stats_table.csv'))


write_csv(combined_sources_zonal_stats_table,
          paste0(hydro_out, 'combined_sources_zonal_stats_table.csv'))


write_csv(harc_sources_zonal_stats_table, 
          paste0(hydro_out, 'harc_water_sources_zonal_stats_table.csv'))


write_csv(NSW_Water_Source_LUT, 
          paste0(hydro_out, 'NSW_Water_Sources_LUT.csv'))




## Create tmap






###################################################################################################
#########################################  ------ TBC ---- ########################################
###################################################################################################
