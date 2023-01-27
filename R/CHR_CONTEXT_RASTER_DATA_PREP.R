######################################################################################
###############################  ------ HARVESTABLE RIGHTS ---- ######################
######################################################################################



## ENVIRONMENT SETTINGS =============================================================


# \ 
# 
# This code coverts the socio-economic data to rasters
#   
#   
#   \

## To-do

## Check ABS




# 4 :: Context Raster data ----


if(rasterize) {
  
  
  ## Water Source geographies
  NSW_CHR_Water_Sources_aggregated <- 
    
    ## Update
    st_read(dsn = paste0(catchment_data, 
                         'CHR_DPEW_Input_Water_Source_Data.gpkg'),
            layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
    filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>% 
    st_transform(., st_crs(8058))
  
  
  NSW_estuaries <- st_read(paste0(DPEW_sayers_data, 
                                  'Estuaries_Numbers11.shp')) %>% 
    
    st_transform(., st_crs(8058)) %>% 
    rename(Est_No1 = AltNumberi)  %>%
    select(Est_No1) %>% 
    
    ## Calculate the areas
    dplyr::mutate(m2       = st_area(geometry),
                  Est_Ha   = units::set_units(m2, value = ha),
                  Est_Ha   = drop_units(Est_Ha),
                  
                  Est_Sqkm = units::set_units(m2, value = km2),
                  Est_Sqkm = drop_units(Est_Sqkm)) %>% 
    select(Est_No1, Est_Ha, Est_Sqkm)
  
  
  HEAVE_WS <- unique(HEAVAE_consequence_combined_max$CHR_Water_Source)
  
  
  NSW_CHR_Water_Sources_NON_HEAVAE <- NSW_CHR_Water_Sources_aggregated %>% 
    
    filter(!CHR_Water_Source %in% HEAVE_WS)
  
  
  # NSW_CHR_WS_LUE_int <- 
  #   
  #   st_read(dsn    = NSW_CHR_WS_Intersections_database_loc,
  #           layer  = 'NSW_CHR_WS_LUE_int') %>%
  #   st_transform(., st_crs(8058)) %>% 
  #   rename(High = high) %>% st_cast(., 'MULTIPOLYGON')
  
  
  
  
  
  ## ABS and DPI Data
  CHR_Context_Tables     <- read_excel_allsheets(paste0(economic_data, 
                                                        'CHR_DPEW_Context_Input_Data.xlsx'))
  SA1_combo              <- CHR_Context_Tables$SA1_combo %>%
    mutate(SA1_11Digit_Code = as.character(SA1_11Digit_Code))
  
  WS_ABS_SA1_combo_index <- CHR_Context_Tables$WS_ABS_SA1_combo_index
  
  NSW_LGA_Water_Combo    <- CHR_Context_Tables$NSW_LGA_Water_Combo %>% 
    select(LGA_NAME_2021, AG_Annual_water_use, Town_Ann_Water_Demand_2014_ML) %>% 
    filter(!is.na(Town_Ann_Water_Demand_2014_ML))
  
  Gross_margins_values   <- CHR_Context_Tables$Gross_margins_values 
  Tourism_businesses     <- CHR_Context_Tables$Tourism_businesses %>% 
    select(LGA_NAME_2021, Tour_Biz_Tot)
  
  
  ## Rasterize the SA1 fields ----
  ## Join the population measures onto the SA1 features
  ABS_CEN2021_SA1_coastal_population <- 
    
    st_read(dsn = paste0(economic_data, 
                         'CHR_NSW_Input_Context_Layers_Data.gpkg'),
            layer = 'ABS_CEN2021_SA1_coastal') %>%  
    
    ## Now join on the Population data
    rename(SA1_11Digit_Code = SA1_CODE21)                     %>%
    mutate(SA1_11Digit_Code = as.character(SA1_11Digit_Code)) %>% 
    left_join(., SA1_combo, by = "SA1_11Digit_Code")
  
  
  ## ABS fields
  SA1_fields <- c("Population", 
                  "NSW_Percentile", 
                  "Agg_Total", 
                  "Unemployed", 
                  "Employed",
                  "Employment_Total",
                  "Agriculture_percent", 
                  "Unemployed_percent")
  
  
  ## Read in the Project feature data here from the inputs geo-database
  SA1_raster_list <- SA1_fields %>%
    
    ## Pipe the list into lapply
    ## val <-  ABS_fields[1]
    lapply(function(val) {
      
      message('Rasterize ', val)
      ABS_raster <- fasterize::fasterize(ABS_CEN2021_SA1_coastal_population, 
                                         csiro.climate.grids.250m[["Annual_mean_temp"]],
                                         field = val)
      
      writeRaster(ABS_raster, paste0(conseq_out, val, '_raster.tif'))
      
    }) %>% c()
  
  ## Rename the list items
  names(SA1_raster_list) <- SA1_fields
  plot(SA1_raster_list[["Agriculture_percent"]])

  
  ## Stack the rasters
  SA1_raster_stack <- raster::stack(SA1_raster_list)
  
  
  names(SA1_raster_list) %>%
    
    ## Pipe the list into lapply
    ## plot <-  names(SA1_raster_list) [1]
    lapply(function(plot) {
      
      (message(plot))
      png(paste0(conseq_out, plot, '_raster.png'),
          10, 6, units = 'in', res = 600)
      
      plot(SA1_raster_list[[plot]], main = plot)
      plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
           border = 'grey', cex = 0.6, add = TRUE)
      
      dev.off()
      
    })
  
  
  ## Rasterize the POA fields ----
  ABS_CEN2021_POA_coastal_Boat_fish <- 
    
    st_read(dsn = paste0(economic_data, 
                         'CHR_NSW_Input_Context_Layers_Data.gpkg'),
            layer = 'ABS_CEN2021_POA_coastal_Boat_fish') %>% 
    st_transform(., st_crs(8058)) %>% 
    
    ## Calculate density - license per unit areas of postcode)
    mutate(Fishing_density = Fish_total/POA_Sqkm,
           Boat_density    = Boat_Total/POA_Sqkm)
  
  
  # Consequence rating based on average annual number of licenses within each 
  # Estuary Catchment. To get number of licenses first calculate density 
  # (license per unit areas of postcode), then rasterize the density, then 
  # zonal stats with CHR Water Source (mean), then multiply by water source 
  # area to get number of license in water source.  Sum for all Water Sources 
  # within an Estuary Catchment. Score of 1-5 obtained for each Estuary Catchment 
  # from a classification method for binning (20 percentiles)
  
  
  ## POA fields
  POA_fields <- c("Fishing_density", "Boat_density")
  
  
  ## Read in the Project feature data here from the inputs geo-database
  POA_raster_list <- POA_fields %>%
    
    ## Pipe the list into lapply
    ## val <-  POA_fields[1]
    lapply(function(val) {
      
      message('Rasterize ', val)
      ABS_raster <- fasterize::fasterize(ABS_CEN2021_POA_coastal_Boat_fish, 
                                         csiro.climate.grids.250m[["Annual_mean_temp"]],
                                         field = val)
      
      writeRaster(ABS_raster, paste0(conseq_out, val, '_raster.tif'))
      
    }) %>% c()
  
  
  ## Rename the list items
  names(POA_raster_list) <- POA_fields

  
  POA_raster_stack <- raster::stack(POA_raster_list)
  
  
  names(POA_raster_list) %>%
    
    ## Pipe the list into lapply
    ## DC <-  DC_plots[1]
    lapply(function(plot) {
      
      png(paste0(conseq_out, plot, '_raster.png'),
          10, 6, units = 'in', res = 600)
      
      plot(POA_raster_list[[plot]], main = plot)
      plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
           border = 'grey', cex = 0.6, add = TRUE)
      
      dev.off()
      
    })
  
  
  ## Rasterize the LGA fields ----
  ABS_CEN2021_LGA_coastal <- 
    
    st_read(dsn = paste0(economic_data, 
                         'CHR_NSW_Input_Context_Layers_Data.gpkg'),
            layer = 'ABS_CEN2021_LGA_coastal') %>%  
    
    mutate(LGA_NAME_2021 = toupper(LGA_NAME_2021), 
           LGA_NAME_2021 = sub(" .*", "", LGA_NAME_2021))  %>% 
    
    ## Now join on the Population data
    left_join(., NSW_LGA_Water_Combo, by = "LGA_NAME_2021") %>% 
    left_join(., Tourism_businesses,  by = "LGA_NAME_2021") %>%
    
    mutate(Tour_Biz_Density = Tour_Biz_Tot/Tot_P_P,
           Water_Demand_Ha  = Town_Ann_Water_Demand_2014_ML/LGA_Ha,
           Water_Demand_Pop = Town_Ann_Water_Demand_2014_ML/Tot_P_P,
           Water_use_Ha     = AG_Annual_water_use/LGA_Ha) %>% 
    
    ## Just get the columns we need
    dplyr::select(LGA_CODE_2021, 
                  LGA_NAME_2021, 
                  LGA_Ha, 
                  LGA_Sqkm,
                  
                  Water_Demand_Ha,
                  Water_Demand_Pop,
                  Water_use_Ha,
                  Tour_Biz_Density,
                  
                  Tot_P_M, Tot_P_F, Tot_P_P, 
                  Age_15_19_yr_P, Age_85ov_P, 
                  
                  Australian_citizen_P, 
                  Birthplace_Elsewhere_P,
                  
                  Lang_used_home_Eng_only_P, 
                  Lang_used_home_Oth_Lang_P,
                  
                  Age_psns_att_edu_inst_25_ov_P, 
                  High_yr_schl_comp_Yr_12_eq_P, 
                  Count_psns_occ_priv_dwgs_P,
                  Indigenous_P_Tot_P) %>% 
    
    mutate(Tot_P_M = as.numeric(Tot_P_M))
  
  
  
  ## ABS fields
  LGA_fields <- c("Water_Demand_Ha", 
                  "Water_Demand_Pop", 
                  "Water_use_Ha",
                  "Tour_Biz_Density", 
                  'Tot_P_M', 
                  'Tot_P_F', 
                  'Tot_P_P', 
                  'Indigenous_P_Tot_P',
                  
                  'Age_15_19_yr_P', 
                  'Age_85ov_P', 
                  
                  'Australian_citizen_P', 
                  'Birthplace_Elsewhere_P', 
                  'Lang_used_home_Eng_only_P', 
                  'Lang_used_home_Oth_Lang_P',
                  
                  'Age_psns_att_edu_inst_25_ov_P', 
                  'High_yr_schl_comp_Yr_12_eq_P', 
                  'Count_psns_occ_priv_dwgs_P')
  
  
  ## Read in the Project feature data here from the inputs geo-database
  LGA_raster_list <- LGA_fields %>%
    
    ## Pipe the list into lapply
    ## val <-  ABS_fields[1]
    lapply(function(val) {
      
      message('Rasterize ', val)
      
      ABS_raster <- fasterize::fasterize(ABS_CEN2021_LGA_coastal, 
                                         csiro.climate.grids.250m[["Annual_mean_temp"]],
                                         field = val)
      
      writeRaster(ABS_raster, paste0(conseq_out, val, '_raster.tif'))
      
    }) %>% c()
  
  ## Rename the list items
  names(LGA_raster_list) <- LGA_fields
  plot(LGA_raster_list[["Water_Demand_Ha"]])

  
  LGA_raster_stack <- raster::stack(LGA_raster_list)
  
  
  names(LGA_raster_list) %>%
    
    ## Pipe the list into lapply
    ## DC <-  DC_plots[1]
    lapply(function(plot) {
      
      png(paste0(conseq_out, plot, '_raster.png'),
          10, 6, units = 'in', res = 600)
      
      plot(LGA_raster_list[[plot]], main = plot)
      plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
           border = 'grey', cex = 0.6, add = TRUE)
      
      dev.off()
      
    })
  
  
  
  ## Rasterize HEVAE MER ----
  
  
  ## Ground water
  HEVAE_WSP_MER <- 
    
    st_read(dsn   = paste0(HEVAE_data, 'HEAVE_Unreg_Coastal_Basin.shp'),
            layer = 'HEAVE_Unreg_Coastal_Basin') %>% 
    st_transform(., st_crs(8058)) %>% 
    
    ## 
    mutate(HEVAE_Unreg_Conseq = as.numeric(Conseque_1)) %>% 
    select(MAJOR_CATC, WATER_SHAR, UNREGULATE, HEVAE_Unreg_Conseq) 
  
  
  HEVAE_CONSQ_MER <- HEVAE_WSP_MER %>% 
    select(HEVAE_Unreg_Conseq) %>% na.omit() 
  
  
  
  ## POA fields
  HEVAE_fields <- c("HEVAE_Unreg_Conseq")
  
  
  ## Read in the Project feature data here from the inputs geo-database
  HEVAE_raster_list <- HEVAE_fields %>%
    
    ## Pipe the list into lapply
    ## val <-  POA_fields[1]
    lapply(function(val) {
      
      message('Rasterize ', val)
      HEVAE_raster <- fasterize::fasterize(HEVAE_CONSQ_MER, 
                                         csiro.climate.grids.250m[["Annual_mean_temp"]],
                                         field = val)
      
      writeRaster(HEVAE_raster, paste0(conseq_out, val, '_raster.tif'))
      
    }) %>% c()
  
  
  ## Rename the list items
  names(HEVAE_raster_list) <- HEVAE_fields
  HEVAE_raster_stack <- raster::stack(HEVAE_raster_list)
  
  
  
  png(paste0(conseq_out, 'HEVAE_raster.png'),
      10, 6, units = 'in', res = 600)
  
  plot(HEVAE_raster_list[["HEVAE_Unreg_Conseq"]], main = 'HEVAE_Unreg_Conseq')
  plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
       border = 'grey', cex = 0.6, add = TRUE)
  
  dev.off()
  
  
  
  ## Rasterize the Eco layers from MER ----
  
  
  ## Ground water
  GroundWater_pressure <- 
    
    st_read(dsn   = paste0(eco_data, 'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
            layer = 'GroundWater_pressure') %>% 
    st_transform(., st_crs(8058))
  
  
  ## POA fields
  GroundWater_fields <- c("M_3_1_scor", "M_12_2_sco")
  
  
  ## Read in the Project feature data here from the inputs geo-database
  GroundWater_raster_list <- GroundWater_fields %>%
    
    ## Pipe the list into lapply
    ## val <-  POA_fields[1]
    lapply(function(val) {
      
      message('Rasterize ', val)
      water_raster <- fasterize::fasterize(GroundWater_pressure, 
                                         csiro.climate.grids.250m[["Annual_mean_temp"]],
                                         field = val)
      
      writeRaster(water_raster, paste0(conseq_out, val, '_raster.tif'))
      
      
    }) %>% c()
  
  
  ## Rename the list items
  names(GroundWater_raster_list) <- GroundWater_fields
  plot(GroundWater_raster_list[["M_3_1_scor"]]);
  plot(GroundWater_raster_list[["M_12_2_sco"]])
  
  
  GroundWater_raster_stack <- raster::stack(GroundWater_raster_list)
  
  
  png(paste0(conseq_out, 'M_3_1_scor_raster.png'),
      10, 6, units = 'in', res = 600)
  
  plot(GroundWater_raster_list[["M_3_1_scor"]], main = "M_3_1_scor")
  plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
       border = 'grey', cex = 0.6, add = TRUE)
  
  dev.off()
  
  
  ## Rasterize Algal Blooms ----
  Algal_Blooms_layer <- 
    
    st_read(dsn   = paste0(eco_data, 'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
            layer = 'Algal_Blooms_layer') %>% 
    st_transform(., st_crs(8058))
  
  
  EST_health_combined_layers <- 
    
    st_read(dsn   = paste0(eco_data, 'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
            layer = 'EST_health_combined_layers') %>% 
    st_transform(., st_crs(8058))
  
  
  ## POA fields
  EST_fields <- c("RISK")
  
  
  ## Read in the Project feature data here from the inputs geo-database
  EST_raster_list <- EST_fields %>%
    
    ## Pipe the list into lapply
    ## val <-  POA_fields[1]
    lapply(function(val) {
      
      message('Rasterize ', val)
      EST_raster <- fasterize::fasterize(EST_health_combined_layers, 
                                         csiro.climate.grids.250m[["Annual_mean_temp"]],
                                         field = val)
      
      writeRaster(EST_raster, paste0(conseq_out, val, '_raster.tif'))
      
    }) %>% c()
  
  
  ## Rename the list items
  names(EST_raster_list) <- EST_fields

  
  EST_raster_stack <- raster::stack(EST_raster_list)
  
  
  png(paste0(conseq_out, 'RISK_raster.png'),
      10, 6, units = 'in', res = 600)
  
  plot(EST_raster_list[["RISK"]], main = "RISK")
  plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
       border = 'grey', cex = 0.6, add = TRUE)
  
  dev.off()
  
  
  
  
  
  ## Rasterize LUE ----

  
  ## POA fields
  # LUE_field <- c("Low", "Mid", "High")
  # 
  # 
  # ## Read in the Project feature data here from the inputs geo-database
  # LUE_raster_list <- LUE_field %>%
  #   
  #   ## Pipe the list into lapply
  #   ## valS <-  POA_fields[1]
  #   lapply(function(val) {
  #     
  #     message('Rasterize ', val)
  #     LUE_raster <- fasterize::fasterize(NSW_CHR_WS_LUE_int, 
  #                                        csiro.climate.grids.250m[["Annual_mean_temp"]],
  #                                        field = val)
  #     
  #     writeRaster(LUE_raster, paste0(conseq_out, val, '_raster.tif'))
  #     
  #   }) %>% c()
  # 
  # 
  # ## Rename the list items
  # names(LUE_raster_list) <- LUE_field
  # 
  # 
  # LUE_raster_stack <- raster::stack(LUE_raster_list)
  # 
  # 
  # names(LUE_raster_list) %>%
  #   
  #   ## Pipe the list into lapply
  #   ## DC <-  DC_plots[1]
  #   lapply(function(plot) {
  #     
  #     png(paste0(conseq_out, plot, '_raster.png'),
  #         10, 6, units = 'in', res = 600)
  #     
  #     plot(LUE_raster_list[[plot]], main = plot)
  #     plot(st_geometry(NSW_CHR_Water_Sources_aggregated), 
  #          border = 'grey', cex = 0.6, add = TRUE)
  #     
  #     dev.off()
  #     
  #   })
  
  
  
  
  ## Extract raster fields to CHR ----
  
  
  message('extracting SA1 raster values to CHR ')
  CHR_SA1_median <- terra::extract(SA1_raster_stack, 
                                   NSW_CHR_Water_Sources_aggregated, 
                                   fun   = median, 
                                   na.rm = TRUE) %>% 
    as_tibble() %>% 
    mutate(CHR_Water_Source = NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)
  
  
  
  message('extracting POA raster values to CHR ')
  CHR_POA_median <- terra::extract(POA_raster_stack, 
                                   NSW_CHR_Water_Sources_aggregated, 
                                   fun   = median, 
                                   na.rm = TRUE) %>% as_tibble() %>% 
    
    mutate(CHR_Water_Source = NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)
  
  
  message('extracting LGA raster values to CHR ')
  CHR_LGA_median <- terra::extract(LGA_raster_stack, 
                                   NSW_CHR_Water_Sources_aggregated, 
                                   fun   = median, 
                                   na.rm = TRUE) %>% as_tibble() %>% 
    
    mutate(CHR_Water_Source = NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)
  
  
  message('extracting Ground water raster values to CHR')
  CHR_GROUNDW_median <- terra::extract(GroundWater_raster_stack, 
                                       NSW_CHR_Water_Sources_aggregated, 
                                       fun   = median, 
                                       na.rm = TRUE) %>% as_tibble() %>% 
    
    mutate(CHR_Water_Source = NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)
  
  
  message('extracting EST health raster values to CHR')
  CHR_EST_median    <- terra::extract(EST_raster_stack, 
                                      NSW_CHR_Water_Sources_aggregated, 
                                      fun   = median, 
                                      na.rm = TRUE) %>% as_tibble() %>% 
    
    mutate(CHR_Water_Source = NSW_CHR_Water_Sources_aggregated$CHR_Water_Source) %>% 
    rename(EST_RISK = V1)
  
  
  message('extracting HEAVAE values to CHR ')
  CHR_HEAVE_MER_median <- terra::extract(HEVAE_raster_stack, 
                                         NSW_CHR_Water_Sources_NON_HEAVAE, 
                                         fun   = median, 
                                         na.rm = TRUE) %>% 
    as_tibble() %>% 
    mutate(CHR_Water_Source = NSW_CHR_Water_Sources_NON_HEAVAE$CHR_Water_Source) %>% 
    rename(HEV_MER = V1) %>% na.omit()
  
  
  
  
  
  ## Save zonal CHR values ----
  ## in some cases, the same zonal tables are needed across different DCs (LGA, SA1)
  message('saving zonal stats to CHR')
  
  
  write_csv(CHR_SA1_median, 
            paste0(DC5_out, 
                   'CHR_SA1_median.csv'))
  
  write_csv(CHR_POA_median, 
            paste0(DC5_out, 
                   'CHR_POA_median.csv'))
  
  write_csv(CHR_LGA_median, 
            paste0(DC6_out, 
                   'CHR_LGA_median.csv'))
  
  write_csv(CHR_LGA_median, 
            paste0(DC7_out, 
                   'CHR_LGA_median.csv'))
  
  write_csv(CHR_LGA_median, 
            paste0(DC8_out, 
                   'CHR_LGA_median.csv'))
  
  write_csv(CHR_SA1_median, 
            paste0(DC8_out, 
                   'CHR_SA1_median.csv'))
  
  write_csv(CHR_GROUNDW_median, 
            paste0(DC2_out, 
                   'CHR_GROUNDW_median.csv'))
  
  write_csv(CHR_EST_median, 
            paste0(DC3_out, 
                   'CHR_EST_median.csv'))
  
  write_csv(CHR_HEAVE_MER_median, 
            paste0(DC1_out, 
                   'CHR_HEAVE_MER_median'))
  
  
}




################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################