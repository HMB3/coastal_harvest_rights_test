############################################################################################
#############################  ---- CALCULATE DAM VOLUMES ---- #############################
############################################################################################


# \ 
# 
# This code calculates the area, volume and estimated volume in ML available for farm dams
# Input data is from Richard Roper ().
#
#   \


## Do each criteria separately.
## Then have a separate script for the combination
harc_dam_int <- FALSE
dam_list     <- TRUE




## SOURCE DATA =============================================================


## 
if(dam_list) {
  
  ## Read in water source geography
  message('calculating the volume of the farm dams ')
  
  
  ## All the water source columns
  NSW_CHR_EMU_LUT <- NSW_CHR_WS_FULL_LUT %>% 
    
    dplyr::select(CHR_Water_Source, EXTRACTION_MANAGEMENT_UNIT) %>% 
    group_by(CHR_Water_Source,      EXTRACTION_MANAGEMENT_UNIT) %>% 
    distinct()
  
  
  ## Filter the WS dataset to just the test EMUs
  NSW_CHR_WS_test <- NSW_CHR_Water_Sources_aggregated %>%
    filter(CHR_Water_Source %in% WS_test)

  plot(st_geometry(NSW_CHR_WS_test))
  
  
  dim(NSW_CHR_Water_Sources_aggregated)
  dim(NSW_CHR_WS_test)
  
  
  NSW_CHR_UWS_aggregated <- st_read(dsn   = DPEW_Water_Source_database_loc,
                                    layer = 'NSW_UWS_aggregated')  
  
  
  ## Aggregated water sources
  NSW_unreg_coastal_water_sources <-
    
    st_read(dsn = DPEW_Water_Source_database_loc,
            layer = 'NSW_unreg_coastal_water_sources_aggregated')
  
  
  NSW_UWS_CHR_WS <- st_read(dsn   = DPEW_Water_Source_database_loc,
                            layer = 'NSW_UWS_CHR_WS_aggregated')
  
  
  ## Update
  HARC_Areas <- st_read(dsn = paste0(hydro_data, 
                                     'HARC/HARC_catchments.shp')) %>% 
    st_transform(., st_crs(8058))  %>% 
    
    dplyr::mutate(m2        = st_area(geometry),
                  Harc_Ha   = units::set_units(m2, value = ha),
                  Harc_Ha   = drop_units(Harc_Ha),
                  
                  Harc_Sqkm = units::set_units(m2, value = km2),
                  Harc_Sqkm = drop_units(Harc_Sqkm)) 
  
  
  
  NSW_CHR_Water_Source_areas <- 
    as_tibble(NSW_CHR_Water_Sources_aggregated) %>% 
    select(-geom)
  
  
  ## Farm dam list
  farm_dams_dir  <- paste0(hydro_data, 
                           "/Farm_Dams/Farm_dams_for_MaxHarvestableRights_221115.gdb")
  
  
  dam_layer_list <- st_layers(paste0(hydro_data, 
                                     "/Farm_Dams/Farm_dams_for_MaxHarvestableRights_221115.gdb")) 
  
  
  ## Create a list of all the farm dams layers - each has 4 layers
  ## Polys, points, etc.
  dam_layers_sf = list() 
  
  
  ## Create list of dam layers ----
  for(i in 1:length(dam_layer_list$name)) {
    
    message('reading in farm layer for ', i)
    dam_layers_sf[[dam_layer_list$name[i]]] <- read_sf(dsn   = farm_dams_dir, 
                                                       layer = dam_layer_list$name[i])
    
  }
  
  
  ## These layers are removed...maybe not what we want...
  dam_subset      <- dam_layers_sf[!grepl('hydroareas_off_SO3_above_EXCL', names(dam_layers_sf))] 
  dam_subset_sort <- names(dam_subset) %>% sort()
  EMU_subset_sort <- sub("\\_.*", "", dam_subset_sort) %>% unique()
  EMU_subset_test <- EMU_subset_sort
  gc()
  
  EMU_dam_code_test <- c(EMU_subset_sort[1], 
                         EMU_subset_sort[19])

  
}





## Errors
# Warning message:
# In CPL_gdalvectortranslate(source, destination, options, oo, doo,  :
# GDAL Message 1: A geometry of type POINT is inserted into layer file28a02edb65af 
# of geometry type MULTIPOLYGON, which is not normally allowed by the GeoPackage 
# specification, but the driver will however do it. To create a conformant GeoPackage, 
# if using ogr2ogr, the -nlt option can be used to override the layer geometry type. 
# This warning will no longer be emitted for this combination of layer and feature geometry type.


# Error in CPL_geos_union(st_geometry(x), by_feature, is_coverage) : 
#   Evaluation error: TopologyException: side location conflict at 9659894.5171722062 4631048.9235903723.
gc()


## Calculate the area, volume & farm contours for each water source ----
if(dam_areas) { 
  
  ## One loop for the
  CHR_farm_dams_area_volume_contours_df <- EMU_subset_sort %>%
    
    ## Pipe the list into l-apply
    # we want to join rows of layers in each set of four EMUs, then we want to bind_rows
    # then we want to bind_rows of layers between each set
    # EMU <- EMU_subset_sort[19]
    lapply(function(EMU) {
      
      message('manipulating farm dam layers for ', EMU)
      EMU_models <- dam_subset_sort[grepl(EMU, dam_subset_sort)] 
      
      ## only use these files :
      # *_final_mga56
      # *_hydropoints_off_SO3_above.
      # *_hydroareas_off_SO3_above_mga56_var_100_9
      
      ## Just join everything to the one feature layer
      dam_feat               <- dam_subset[[EMU_models[1]]] %>% 
        filter(LandUseFactor > 0)     %>% 
        st_transform(., st_crs(8058)) %>% ensure_multipolygons() %>% 
        st_make_valid() %>% st_buffer(., 0.0) 
      
      dam_UWS       <- unique(dam_feat$UNREGULATED_WATER_SOURCE)
      
      ## Try reducing the precision to speed up the intersect
      ## Key here was using the right DPE geography - with the WS and UWS
      NSW_CHR_UWS_EMU <- NSW_unreg_coastal_water_sources %>% 
        .[.$UNREGULATED_WATER_SOURCE %in% dam_UWS, ]     %>% 
        st_set_precision(units::set_units(10, nm))       %>% st_make_valid()
      
      NSW_CHR_WUS_EMU_WS <- NSW_UWS_CHR_WS           %>%
        .[.$UNREGULATED_WATER_SOURCE %in% dam_UWS, ] %>% 
        st_set_precision(units::set_units(10, nm))   %>% st_make_valid()
      
      NSW_CHR_WUS_EMU_WS_df <- NSW_CHR_WUS_EMU_WS %>% 
        as_tibble     %>% 
        select(-geom) %>% distinct()
      
      message('get inersection of Dam AREAs and the CHR UWS - slow...')
      dam_feat_CHR  <- st_intersection(st_make_valid(dam_feat), 
                                       st_make_valid(NSW_CHR_UWS_EMU))
      gc()
      
      
      ## save out the intersect for cross-checking
      if(nrow(dam_feat_CHR) > 0) {
        
        png(paste0(hydro_out, 'dams/',  EMU, '_Dam_CHR_harc_areas.png'),
            10, 6, units = 'in', res = 500)
        
        plot(st_geometry(dam_feat_CHR),      border = 'grey')
        plot(st_geometry(NSW_CHR_Water_Sources_aggregated),    border = 'red', add = TRUE)
        plot(st_geometry(NSW_CHR_UWS_EMU), border = 'blue', add = TRUE)
        
        title(main = EMU, 
              # xlab = "X axis", ylab = "Y axis",
              cex.main = 4,   font.main = 3, col.main = "darkgreen",
              cex.sub = 2, font.sub = 3, col.sub = "darkgreen",
              col.lab ="black")
        
        dev.off()
        
      }
      
      
      message('get inersection of Dam POINTS and the CHR UWS - slow...')
      dam_hydro_ar_100_9     <- dam_subset[[EMU_models[2]]] %>% st_transform(., st_crs(8058)) %>%
        st_make_valid() %>% 
        st_intersection(., st_make_valid(NSW_CHR_UWS_EMU))
      
      dam_hydro_pt_SO3_above <- dam_subset[[EMU_models[4]]] %>% st_transform(., st_crs(8058)) %>% 
        st_make_valid() %>% 
        st_intersection(., st_make_valid(NSW_CHR_UWS_EMU))
      gc()
      
      ## save out the intersect for cross-checking
      if(nrow(dam_hydro_ar_100_9)     > 0 &
         nrow(dam_hydro_pt_SO3_above) > 0 &
         nrow(NSW_CHR_UWS_EMU)        > 0 ) {
        
        png(paste0(hydro_out, 'dams/',  EMU, '_Dam_CHR_harc_points.png'),
            10, 6, units = 'in', res = 500)
        
        plot(st_geometry(dam_hydro_ar_100_9),               border = 'red',    cex = 3)
        plot(st_geometry(NSW_CHR_Water_Sources_aggregated), border = 'grey',   add = TRUE)
        plot(st_geometry(dam_hydro_pt_SO3_above),           border = 'orange', add = TRUE, cex = 0.6)
        plot(st_geometry(NSW_CHR_UWS_EMU),                  border = 'blue',   add = TRUE)
        
        dev.off()
        
      }
      
      # Area of water source (already there)
      # Area of land use for each land-use factor in each water source
      # Sum Volume (ML) of water from hydro-point
      # Median of contour value (raster) - only one per group of 4
      message('Calculate the Area per water source for ', EMU)
      dam_area <- dam_feat_CHR %>% 
        
        dplyr::select(CHR_Water_Source,
                      WATER_SHARING_PLAN, 
                      UNREGULATED_WATER_SOURCE, 
                      EXTRACTION_MANAGEMENT_UNIT) %>% 
        
        mutate(Dam_m2   = st_area(.),
               Dam_Ha   = units::set_units(Dam_m2, value = ha),
               Dam_Ha   = drop_units(Dam_Ha),
               
               Dam_Sqkm = units::set_units(Dam_m2, value = km2),
               Dam_Sqkm = drop_units(Dam_Sqkm)) %>% 
        
        group_by(CHR_Water_Source) %>%
        
        summarise(Dam_Ha   = sum(Dam_Ha), 
                  Dam_Sqkm = sum(Dam_Sqkm)) %>% 
        mutate(EMU_model   = paste0(EMU, '_area_final')) %>% 
        
        dplyr::select(CHR_Water_Source, 
                      EMU_model, 
                      Dam_Ha, 
                      Dam_Sqkm)
      gc()
      
      ## Get the tibble too
      dam_area_df <- dam_area %>% as_tibble() %>% select(-geom)
      write_csv(dam_area_df,
                paste0(hydro_out, 'dams/', EMU, '_dam_area_volume_contour_df.csv'))
      
      ## If there is an additional intersect, calculate the areas....
      if(harc_dam_int) {
        if(nrow(dam_feat_harc_int) > 0) {
          
          message('Dam areas intersect with Harc, so calculate additional intersection for ', EMU)
          dam_area_CHR_harc <- dam_feat_harc_int %>%
            
            dplyr::select(WATER_SHARING_PLAN,
                          UNREGULATED_WATER_SOURCE,
                          EXTRACTION_MANAGEMENT_UNIT) %>%
            
            mutate(Dam_harc_m2   = st_area(.),
                   Dam_harc_Ha   = units::set_units(Dam_harc_m2, value = ha),
                   Dam_harc_Ha   = drop_units(Dam_harc_Ha),
                   
                   Dam_harc_Sqkm = units::set_units(Dam_harc_m2, value = km2),
                   Dam_harc_Sqkm = drop_units(Dam_harc_Sqkm)) %>%
            
            group_by(UNREGULATED_WATER_SOURCE) %>%
            
            summarise(Dam_harc_Ha   = sum(Dam_harc_Ha),
                      Dam_harc_Sqkm = sum(Dam_harc_Sqkm)) %>%
            mutate(EMU_model   = paste0(EMU, '_area_final')) %>%
            
            dplyr::select(UNREGULATED_WATER_SOURCE,
                          EMU_model,
                          Dam_harc_Ha,
                          Dam_harc_Sqkm)
          
          dam_area_CHR_harc_df <- dam_area_CHR_harc %>% as_tibble() %>%
            select(-geometry)
          
          write_csv(dam_area_CHR_harc_df,
                    paste0(hydro_out, 'dams/',  EMU, '_dam_area_CHR_harc_df.csv'))
          
          dam_area_CHR_harc_combo <- bind_rows(dam_area,
                                               dam_area_CHR_harc)
          
          dam_area_CHR_harc_combo_df <- dam_area_CHR_harc_combo %>% as_tibble() %>%
            select(-geom, -geometry)
          
          write_csv(dam_area_CHR_harc_combo_df,
                    paste0(hydro_out, 'dams/',  EMU, '_dam_area_CHR_harc_combo_df.csv'))
          
          gc()
          
          ## If there is an additional intersect, calculate the areas....
        } else {
          message('Dam areas do not intersect with Harc, no additional areas for ', EMU)
        }
      }
      
      # Sum of Volume (ML) of water from hydro-area 100_9
      dam_hydro_ar_100_9_vol <- dam_hydro_ar_100_9 %>% 
        
        as_tibble() %>% 
        dplyr::select(CHR_Water_Source, 
                      Volume_ML) %>% 
        group_by(CHR_Water_Source)    %>%
        
        summarise(Volume_ML = sum(Volume_ML)) %>% 
        mutate(EMU_model    = paste0(EMU, '_area_100_9')) %>% 
        dplyr::select(CHR_Water_Source, EMU_model, Volume_ML)
      
      ## Sum of Volume (ML) of water from hydro-points 100_9
      dam_hydro_pt_SO3_above <- dam_hydro_pt_SO3_above %>% 
        
        as_tibble() %>% 
        dplyr::select(CHR_Water_Source, 
                      Volume_ML) %>% 
        group_by(CHR_Water_Source) %>%
        
        summarise(Volume_ML = sum(Volume_ML)) %>% 
        mutate(EMU_model    = paste0(EMU, '_point_SO3_above')) %>% 
        dplyr::select(CHR_Water_Source, EMU_model, Volume_ML)
      
      ## Then get the contour values
      ## https://stackoverflow.com/questions/69144772/exact-extract-unsupported-geometry-type-error
      message('Calculate the Contour zonal stats for ', EMU)
      dam_area$geom_type <- st_geometry_type(dam_area, by_geometry = TRUE)
      dam_poly           <- dam_area %>% st_cast(., "MULTIPOLYGON")
      Contour_med        <- exact_extract(harvest_contours, 
                                          dam_poly, 
                                          fun   = 'median') %>% as_tibble()
      
      names(Contour_med)           <- c('Contour_median')
      Contour_med$CHR_Water_Source <- dam_area$CHR_Water_Source
      dam_area_contour             <- bind_cols(dam_area, Contour_med[1]) %>% 
        ensure_multipolygons()
      gc()
      
      dam_volume  <- list(dam_hydro_ar_100_9_vol, 
                          dam_hydro_pt_SO3_above) %>% 
        
        reduce(full_join, by = c('CHR_Water_Source', 'EMU_model', 'Volume_ML')) %>% 
        rename(EMU_model_volume = EMU_model)
      
      
      ## Update
      dam_area_volume_contour <- full_join(dam_area_contour, dam_volume,
                                           by = c("CHR_Water_Source")) %>% 
        st_make_valid() %>% 
        dplyr::select(CHR_Water_Source, 
                      EMU_model, 
                      EMU_model_volume,
                      Dam_Ha, 
                      Dam_Sqkm, 
                      Contour_median, 
                      Volume_ML)
      
      ## Save the full set out
      dam_area_volume_contour_df <- as_tibble(dam_area_volume_contour) %>% select(-geom) 
      write_csv(dam_area_volume_contour_df,
                paste0(hydro_out, 'dams/',  EMU, '_dam_area_volume_contour_df.csv'))
      
      message ('final CHR WS Aggregation ')
      dam_area_volume_contour_CHR <- dam_area_volume_contour %>% 
        
        ## Group by
        group_by(CHR_Water_Source) %>% summarize(Volume_ML      = sum(Volume_ML),
                                                 Dam_Ha         = median(Dam_Ha),
                                                 Dam_Sqkm       = median(Dam_Sqkm),
                                                 Contour_median = median(Contour_median)) %>% 
        
        ## Note Zero volumes are those with insufficient dimensions...
        ## Of those that TOUCH the edges of the DAM layer for that EMU
        as_tibble() %>%  
        dplyr::select(CHR_Water_Source, 
                      Dam_Ha, 
                      Dam_Sqkm,
                      Volume_ML, 
                      Contour_median) %>% 
        arrange(CHR_Water_Source)
      
      
      ## We only want the rows where the CHR_Water_Source == UNREGULATED_WATER_SOURCE.
      ## That's because the st_intersects includes areas that touch/
      ## st_intersects, and then CLIP by the key layer (WS or DAM) would be best
      
    }) %>% dplyr::bind_rows(.)
  
  gc()
  
  ## Save Summary table
  write_csv(CHR_farm_dams_area_volume_contours_df,
            paste0(hydro_out, 'dams/', 'NSW_CHR_WS_Farm_Dam_Areas.csv'))
  
  
  NSW_CHR_WS_Farm_Dam_AGG_Areas <- CHR_farm_dams_area_volume_contours_df %>% 
    
    replace(is.na(.), 0) %>% 
    
    ## The Group by above hasn't quite worked...need to find why 
    group_by(CHR_Water_Source) %>% summarize(Volume_ML      = sum(Volume_ML),
                                             Dam_Ha         = sum(Dam_Ha),
                                             Dam_Sqkm       = sum(Dam_Sqkm),
                                             Contour_median = sum(Contour_median))
  
  write_csv(NSW_CHR_WS_Farm_Dam_AGG_Areas,
            paste0(hydro_out, 'dams/', 'NSW_CHR_WS_Farm_Dam_AGG_Areas.csv'))
  
}






################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################