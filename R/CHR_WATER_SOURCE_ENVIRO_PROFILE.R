###################################################################################################
###############################  ------ WATER SOURCE PROFILE ---- #################################
###################################################################################################


# \ 
# 
# This code creates an enviro profile of each water source
#   
#   
#   \


## ENVIRONMENT SETTINGS =============================================================


## DPE/Petter to provide LUT between HARC and water sources
## match harc_annual_impact_100_df  to match NSW_water_sources



# STEP 1 :: read data ----


## 
if(enviro_summary) {
  
  
  ## All the water source columns
  NSW_CHR_EMU_LUT <- 
    read_csv(paste0(data_dir, 
                    'NSW_CHR_Water_Sources_LUT.csv')) %>% 
    
    dplyr::select(CHR_Water_Source, EXTRACTION_MANAGEMENT_UNIT) %>% 
    group_by(CHR_Water_Source,      EXTRACTION_MANAGEMENT_UNIT) %>% 
    distinct()
  
  
  NSW_CHR_Water_Sources_aggregated <- 
    
    st_read(dsn = paste0(data_dir, 
                         'CHR_DPEW_Input_Water_Source_Data.gpkg'),
            layer = 'NSW_CHR_Water_Sources_aggregated') 
  
  
  ## Get the stream density
  NSW_water_sources_stream_density <- 
    read_csv(paste0(data_dir, 'NSW_water_sources_stream_density.csv'))
  
  
  NSW_harc_stream_density <- 
    read_csv(paste0(data_dir, 'NSW_harc_stream_density.csv'))
  
  
  ## Update
  HARC_Areas <- st_read(dsn = paste0(hydro_data, 
                                     'HARC/HARC_catchments.shp')) %>% 
    st_transform(., st_crs(8058)) %>% 
    
    dplyr::mutate(m2   = st_area(geometry),
                  Harc_Ha   = units::set_units(m2, value = ha),
                  Harc_Ha   = drop_units(Harc_Ha),
                  
                  Harc_Sqkm = units::set_units(m2, value = km2),
                  Harc_Sqkm = drop_units(Harc_Sqkm))  
  
  
  ## Aggregated water sources
  NSW_unreg_coastal_water_sources <-
    
    st_read(dsn   = paste0(data_dir, 'CHR_DPEW_Input_Water_Source_Data.gpkg'),
            layer = 'NSW_unreg_coastal_water_sources') 
  
  
  ## Use BEGA and Brogo
  length(unique(NSW_CHR_EMU_LUT$EXTRACTION_MANAGEMENT_UNIT)) ## 65 EMUs
  EMU_list   <- unique(NSW_CHR_EMU_LUT$EXTRACTION_MANAGEMENT_UNIT) %>% sort()
  EMU_test   <- c(EMU_list[6])
  WS_test    <- NSW_CHR_EMU_LUT %>% 
    # filter(EXTRACTION_MANAGEMENT_UNIT == EMU_test) %>% 
    .$CHR_Water_Source %>% unique() %>% sort()
  
  
  
  ## Subset the full WS to the test EMUs
  NSW_EMU_test_area <- 
    
    NSW_unreg_coastal_water_sources %>% 
    filter(EXTRACTION_MANAGEMENT_UNIT == EMU_test) %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(EXTRACTION_MANAGEMENT_UNIT) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() %>% 
    
    dplyr::mutate(EMU_m2   = st_area(geometry),
                  EMU_Ha   = units::set_units(EMU_m2, value = ha),
                  EMU_Ha   = drop_units(EMU_Ha),
                  
                  EMU_Sqkm = units::set_units(EMU_m2, value = km2),
                  EMU_Sqkm = drop_units(EMU_Sqkm))
  
  ## Just the EMUs
  NSW_EMU_test_units <- NSW_EMU_test_area %>% 
    dplyr::select(EXTRACTION_MANAGEMENT_UNIT)
  
  
  ## Subset the aggregated WS to the test EMUs 
  NSW_CHR_Water_Sources_aggregated_test <- NSW_CHR_Water_Sources_aggregated %>% 
    
    .[.$CHR_Water_Source %in% WS_test, ] %>% 
    st_intersection(., NSW_EMU_test_units)
  
  
  plot(st_geometry(NSW_CHR_Water_Sources_aggregated_test))
  plot(st_geometry(NSW_EMU_test_units))
  plot(st_geometry(NSW_CHR_Water_Sources_aggregated_test), add = TRUE)
  
  
  
  ## Check enviro rasters
  plot_rasters <- stack(water.source.grids.250m[['Max_temp_warm_month']],
                        water.source.grids.250m[['Mean_diurnal_range']],
                        water.source.grids.250m[['Min_temp_cold_month']],
                        water.source.grids.250m[['Temp_annual_range']])
  
  
  ckey <- list(labels = list(cex = 2))
  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  
  levelplot(plot_rasters,
            col.regions = terrain.colors(10),
            layout      = c(2, 2),
            colorkey    = ckey)
  
  
  
  
  # STEP 2 :: Calculate zonal stats ----
  
  
  
  ## If we are doing the zonal stats
  water_sources_zonal_stats_df <- WS_test %>%
    
    ## Pipe the list into lapply
    #source = WS_test[1]
    lapply(function(source) {
      
      message('calculate zonal stats for ', source)
      
      ## subset the source
      source_data <- NSW_CHR_Water_Sources_aggregated %>% 
        .[.$CHR_Water_Source %in% source, ] #%>% 
      # dplyr::select(-WS_Hectares, -WS_Sq_km)
      
      if(nrow(source_data) >= 1) {
        
        ## Extract the raster values for that Water_source
        zonal_median <- exact_extract(water.source.grids.250m, 
                                      source_data, 
                                      fun   = 'median') %>% as_tibble()
        
        zonal_max    <- exact_extract(water.source.grids.250m, 
                                      source_data, 
                                      fun   = 'max') %>% as_tibble() 
        
        zonal_min    <- exact_extract(water.source.grids.250m, 
                                      source_data, 
                                      fun   = 'min') %>% as_tibble()
        
        ## Can also use rasters with different resolutions if needed
        zonal_data <- zonal_median %>% 
          bind_cols(., zonal_max) %>% 
          bind_cols(., zonal_min)
        
        ## Can also use rasters with different resolutions if needed.
        
        ## Bind the data together
        source_data_zonal <- source_data %>% 
          bind_cols(., zonal_data) %>% as_tibble() %>% select(-geom)
        
      } else {
        message('Check source data ', source, ' skip')
      }
      
      ## This should be distinct -check later
    }) %>% dplyr::bind_rows(.) %>% as_tibble() 
  
  
  
  
  
  EMU_zonal_stats_df <- EMU_test %>%
    
    ## Pipe the list into lapply
    #emu = EMU_test[1]
    lapply(function(emu) {
      
      message('calculate zonal stats for ', emu)
      
      ## subset the emu
      emu_data <- NSW_EMU_test_area %>% 
        filter(EXTRACTION_MANAGEMENT_UNIT == emu) #%>% 
      #dplyr::select(-EMU_Hectares, -EMU_Sq_km)
      
      if(nrow(emu_data) >= 1) {
        
        ## Extract the raster values for that Water_emu
        zonal_median <- exact_extract(water.source.grids.250m, 
                                      emu_data, 
                                      fun   = 'median') %>% as_tibble()
        
        zonal_max    <- exact_extract(water.source.grids.250m, 
                                      emu_data, 
                                      fun   = 'max') %>% as_tibble() 
        
        zonal_min    <- exact_extract(water.source.grids.250m, 
                                      emu_data, 
                                      fun   = 'min') %>% as_tibble()
        
        ## Can also use rasters with different resolutions if needed
        zonal_data <- zonal_median %>% 
          bind_cols(., zonal_max) %>% 
          bind_cols(., zonal_min)
        
        ## Can also use rasters with different resolutions if needed.
        
        ## Bind the data together
        emu_zonal_data <- emu_data %>% 
          
          bind_cols(., zonal_data) %>% as_tibble() %>% select(-geometry)
        
      } else {
        message('Check emu data ', emu, ' skip')
      }
      
      ## This should be distinct -check later
    }) %>% dplyr::bind_rows(.) %>% as_tibble() 
  
  
  
  
  
  ## Calc the zonal stats for the HARC areas
  HARC_zonal_stats_df <- unique(HARC_Areas$GhostName) %>%
    
    ## Pipe the list into lapply
    #emu = EMU_test[1]
    lapply(function(harc) {
      
      message('calculate zonal stats for ', harc)
      
      ## subset the emu
      harc_data <- HARC_Areas %>% 
        filter(GhostName == harc) 
      
      if(nrow(harc_data) >= 1) {
        
        ## Extract the raster values for that Water_emu
        zonal_median <- exact_extract(water.source.grids.250m, 
                                      harc_data, 
                                      fun   = 'median') %>% as_tibble()
        
        zonal_max    <- exact_extract(water.source.grids.250m, 
                                      harc_data, 
                                      fun   = 'max') %>% as_tibble() 
        
        zonal_min    <- exact_extract(water.source.grids.250m, 
                                      harc_data, 
                                      fun   = 'min') %>% as_tibble()
        
        ## Can also use rasters with different resolutions if needed
        zonal_data <- zonal_median %>% 
          bind_cols(., zonal_max) %>% 
          bind_cols(., zonal_min)
        
        ## Can also use rasters with different resolutions if needed.
        
        ## Bind the data together
        harc_zonal_data <- harc_data %>% 
          
          bind_cols(., zonal_data) %>% as_tibble() %>% select(-geometry)
        
      } else {
        message('Check emu data ', harc, ' skip')
      }
      
      ## This should be distinct -check later
    }) %>% dplyr::bind_rows(.) %>% as_tibble() 
  
  
  
  # STEP 3 :: Save data ----
  
  
  ## Combine the spatial data
  NSW_CHR_Water_Sources_enviro_profile <- water_sources_zonal_stats_df %>% 
    left_join(., NSW_water_sources_stream_density, by = "CHR_Water_Source") %>%  
    
    select(CHR_Water_Source, Hectares, Sq_km,
           Stream_length_km, Stream_density_Sqkm, everything())
  
  
  NSW_CHR_HARC_enviro_profile <- HARC_zonal_stats_df %>% 
    left_join(., NSW_harc_stream_density, by = "GhostName") %>%  
    
    select(MAJOR_CATC, GhostName,
           Harc_Stream_length_km, 
           Harc_Stream_density_Sqkm,
           everything())
  
  write_csv(NSW_CHR_Water_Sources_enviro_profile, 
            paste0(enviro_out, 'NSW_CHR_Water_Sources_enviro_profile.csv'))
  
  write_csv(EMU_zonal_stats_df, 
            paste0(enviro_out, 'NSW_CHR_EMU_enviro_profile.csv'))
  
  write_csv(NSW_CHR_HARC_enviro_profile, 
            paste0(enviro_out, 'NSW_CHR_HARC_enviro_profile.csv'))
  
  gc()
  
}





###################################################################################################
#########################################  ------ TBC ---- ########################################
###################################################################################################