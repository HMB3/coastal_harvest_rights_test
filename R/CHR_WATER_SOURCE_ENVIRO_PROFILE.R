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
enviro_summary <- FALSE
zonal_stats    <- FALSE


if(zonal_stats) {
  
  
  ## All the water source columns
  NSW_CHR_EMU_LUT <- 
    
    read_excel_allsheets(paste0(catchment_data, 
                                'CHR_WS_NUM_HEVAE_LUT_Data.xlsx'))$NSW_CHR_WS_FULL_LUT %>% 
    
    dplyr::select(CHR_Water_Source, EXTRACTION_MANAGEMENT_UNIT) %>% 
    group_by(CHR_Water_Source,      EXTRACTION_MANAGEMENT_UNIT) %>% 
    distinct()
  
  
  NSW_CHR_Water_Sources_aggregated <- 
    
    st_read(dsn = DPEW_Water_Source_database_loc,
            layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
    dplyr::filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')
  
  
  NSW_CHR_EMU_aggregated <- 
    
    st_read(dsn = DPEW_Water_Source_database_loc,
            layer = 'NSW_CHR_EMU_aggregated')
  
  
  ## Get the stream density
  NSW_water_sources_stream_density <- 
    read_csv(paste0(data_dir, 'NSW_water_sources_stream_density.csv'))
  
  
  NSW_harc_stream_density <- 
    read_csv(paste0(data_dir, 'NSW_harc_stream_density.csv'))
  
  
  NSW_EMU_stream_density <- read_csv(paste0(catchment_data,
                                            'NSW_EMU_stream_density.csv')) %>% 
    na.omit()
  
  NSW_EMU_areas <- read_csv(paste0(catchment_data,
                                   'NSW_CHR_EMU_Area.csv'))
  
  
  ## Aggregated water sources
  NSW_unreg_coastal_water_sources <-
    
    st_read(dsn   =  DPEW_Water_Source_database_loc,
            layer = 'NSW_unreg_coastal_water_sources') 
  
 
}


# STEP 2 :: Calculate zonal stats ----


## WATER SOURCE ----
if(zonal_stats) {
  
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
  
  
  
  
  ## EMU STATS ----
  EMU_zonal_stats_df <- unique(NSW_CHR_EMU_aggregated$EXTRACTION_MANAGEMENT_UNIT) %>%
    
    ## Pipe the list into lapply
    #emu = EMU_test[1]
    lapply(function(emu) {
      
      message('calculate zonal stats for ', emu)
      
      ## subset the emu
      emu_data <- NSW_CHR_EMU_aggregated %>% 
        dplyr::filter(EXTRACTION_MANAGEMENT_UNIT == emu) 
      
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
          
          bind_cols(., zonal_data) %>% as_tibble() %>% select(-geom)
        
      } else {
        message('Check emu data ', emu, ' skip')
      }
      
      ## This should be distinct -check later
    }) %>% dplyr::bind_rows(.) %>% as_tibble() %>% na.omit()
  
  
  
  
  
  ## HARC STATS  ----
  HARC_zonal_stats_df <- unique(HARC_Areas$GhostName) %>%
    
    ## Pipe the list into lapply
    #emu = EMU_test[1]
    lapply(function(harc) {
      
      message('calculate zonal stats for ', harc)
      
      ## subset the emu
      harc_data <- HARC_Areas %>% 
        dplyr::filter(GhostName == harc) 
      
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
          bind_cols(., zonal_max)  %>% 
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
  NSW_CHR_Water_Sources_enviro_profile <- water_sources_zonal_stats_df     %>% 
    
    left_join(., NSW_water_sources_stream_density, by = "CHR_Water_Source") %>%  
    select(CHR_Water_Source, CHR_Ha, CHR_Sqkm,
           Stream_length_km, Stream_density_Sqkm, everything())
  
  
  NSW_CHR_HARC_enviro_profile <- HARC_zonal_stats_df        %>% 
    
    left_join(., NSW_harc_stream_density, by = "GhostName") %>%  
    select(MAJOR_CATC, GhostName,
           Harc_Stream_length_km, 
           Harc_Stream_density_Sqkm,
           everything())
  
  
  NSW_CHR_EMU_enviro_profile <- EMU_zonal_stats_df %>%
    
    left_join(., NSW_EMU_stream_density, by = "EXTRACTION_MANAGEMENT_UNIT") %>%  
    left_join(., NSW_EMU_areas,          by = "EXTRACTION_MANAGEMENT_UNIT") %>%
    
    select(EXTRACTION_MANAGEMENT_UNIT, 
           EMU_Ha, 
           EMU_Sqkm,
           EMU_Stream_length_km, 
           EMU_Stream_density_Sqkm, 
           everything())
  
  
  write_csv(NSW_CHR_Water_Sources_enviro_profile, 
            paste0(enviro_out, 'NSW_CHR_WS_PROFILE.csv'))
  
  write_csv(NSW_CHR_EMU_enviro_profile, 
            paste0(enviro_out, 'NSW_CHR_EMU_PROFILE.csv'))
  
  write_csv(NSW_CHR_HARC_enviro_profile, 
            paste0(enviro_out, 'NSW_CHR_HARC_PROFILE.csv'))
  
  gc()
  
}







###################################################################################################
#########################################  ------ TBC ---- ########################################
###################################################################################################