######################################################################################
###############################  ------ HARVESTABLE RIGHTS ---- ######################
######################################################################################



## ENVIRONMENT SETTINGS =============================================================


# \ 
# 
# This code does the big data intersects needed to calculate the
#   
#   
#   \

## To-do ----


## Remove any line strings from the intersects
## 


EST  <- TRUE
CARB <- TRUE
NTT  <- FALSE
GM   <- FALSE
REP  <- TRUE



# 4 :: Context Raster data ----


## Intersections ----
if(conseq_inter) {
  
  
  
  ## Read in water source geographies
  dim(NSW_CHR_Water_Sources_aggregated)
  
  
  
  
  
  ## DC 1.1 ----
  
  
  ## no slow intersections for this measure
  
  
  ## DC 1.3 ----
  
  
  NSW_estuaries <- st_read(paste0(DPEW_sayers_data, 
                                  'Estuaries_Numbers11.shp')) %>% 
    
    st_transform(., st_crs(8058)) %>% 
    rename(Est_No1 = AltNumberi)  %>%
    select(Est_No1) 
  
  
  ## Macro-phytes - sea grasses and mangroves 
  NSW_MACROPH <- st_read(dsn = NSW_Ecological_Layers_database_loc,
                         layer = 'NSW_MACROPH') %>% 
    filter(!is.na(MACROPHYTE))
  
  
  ## Macrophytes - sea grasses and mangroves.
  ## This doesn't overlap that much...doesn't make sense to crop it
  NSW_BLUE_CARB <- st_read(dsn   = NSW_Ecological_Layers_database_loc,
                           layer = 'NSW_BLUE_CARB') %>%
    
    ## This might not be needed
    dplyr::select(BLUE_CARB_Raster, geom) 
  
  
  NSW_BLUE_CARB_agg <- 
    
    st_read(dsn = NSW_Ecological_Layers_database_loc,
            layer = 'NSW_BLUE_CARB_agg')
  
  if(EST) {
    
    
    ESTUARY_macrophytes_int <- 
      
      st_intersection(st_make_valid(NSW_estuaries),   
                      st_make_valid(NSW_MACROPH)) 
    
    
    message('writing LUE-CHR Intersect to geo-package')
    st_write(ESTUARY_macrophytes_int,
             dsn    = NSW_CHR_WS_Intersections_database_loc,
             layer  = 'ESTUARY_macrophytes_int',
             quiet  = TRUE,
             append = TRUE,
             delete_dsn = TRUE)
    
  }
  
  
  
  ## DC 1.4 ----
  
  if(CARB) {
    
    ESTUARY_blue_carbon_int <- 
      
      ## The Zonal stats might be better than this...
      st_intersection(st_make_valid(NSW_estuaries),   
                      st_make_valid(NSW_BLUE_CARB_agg))
    
    message('writing EST-Blue Carbon Intersect to geo-package')
    st_write(ESTUARY_blue_carbon_int,
             dsn    = NSW_CHR_WS_Intersections_database_loc,
             layer  = 'ESTUARY_blue_carbon_int',
             quiet  = TRUE,
             append = TRUE,
             delete_dsn = TRUE)
    
    
    Water_source_blue_carbon_int <- 
      
      ## The Zonal stats might be better than this...
      st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                      st_make_valid(NSW_BLUE_CARB_agg))
    
    message('writing WS-Blue Carbon Intersect to geo-package')
    st_write(Water_source_blue_carbon_int,
             dsn        = NSW_CHR_WS_Intersections_database_loc,
             layer      = 'NSW_CHR_WS_blue_carbon_int',
             quiet      = TRUE,
             append     = TRUE,
             delete_dsn = TRUE)
    
  }
  
  
  
  
  
  ## DC 4.1 -----
  
  
  if(NTT) {
    
    
    NSW_NTT_DET <- 
      
      st_read(dsn = NSW_Context_Layers_database_loc,
              layer = 'NSW_NTT_DET') %>% 
      st_transform(., st_crs(8058)) 
    
    
    message('intersecting NTT with CHR') 
    NSW_CHR_NTT_int <- 
      
      st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                      st_make_valid(NSW_NTT_DET))
    
    message('Writing Water Sources and NTT Intersect')
    st_write(NSW_CHR_NTT_int,
             dsn        = NSW_CHR_WS_Intersections_database_loc,
             layer      = 'NSW_CHR_NTT_int',
             quiet      = TRUE,
             append     = TRUE,
             delete_dsn = TRUE)
    
  }
  
  
  ## DC 8.2 ----
  
  
  if(GM) {
    
    ## Update
    NSW_2ND_LUE_GM <- st_read(dsn = NSW_Context_Layers_database_loc,
                              layer = 'NSW_2ND_LUE_GM')
    
    NSW_CHR_EMU <-
      
      st_read(dsn = DPEW_Water_Source_database_loc,
              layer = 'NSW_CHR_EMU_aggregated')
    
    message('Intersecting Water Sources and Land Use')
    NSW_CHR_WS_LUE_int <- 
      
      st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                      st_make_valid(NSW_2ND_LUE_GM)) 
    
    
    message('Writing Water Sources and Land Use aIntersect')
    st_write(NSW_CHR_WS_LUE_int,
             dsn        = NSW_CHR_WS_Intersections_database_loc,
             layer      = 'NSW_CHR_WS_LUE_int',
             quiet      = TRUE,
             append     = TRUE)
    
    NSW_CHR_EMU_LUE_int <- 
      
      st_intersection(st_make_valid(NSW_CHR_EMU),   
                      st_make_valid(NSW_2ND_LUE_GM)) 
    
    
    message('Writing Water Sources and Land Use aIntersect')
    st_write(NSW_CHR_EMU_LUE_int,
             dsn        = NSW_CHR_WS_Intersections_database_loc,
             layer      = 'NSW_CHR_EMU_LUE_int',
             quiet      = TRUE,
             append     = TRUE)
    
  }
  
  
  
  ## EMU REPORT INTERSECTIONS ----
  
  if(REP) {
    
    # EMU_models   <- dam_subset_sort[grepl(EMU_subset_sort[1], dam_subset_sort)] 
    # EMU_dam_feat <- dam_subset[[EMU_models[1]]] %>%
    # 
    #   st_transform(., st_crs(8058)) %>%
    #   .[NSW_CHR_Water_Sources_aggregated_test, ] %>%
    #   st_intersection(., NSW_CHR_Water_Sources_aggregated_test) %>%
    #   filter(LandUseFactor > 0) %>%
    #   mutate(Dams_Area = 'Landuse_Dam') %>%
    # 
    #   ## This creates slivers, which need to be removed...
    #   group_by(Dams_Area) %>%
    #   summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>%
    #   nngeo::st_remove_holes()
    
    # EMU_dam_code_test <- c(EMU_subset_sort[1], EMU_subset_sort[19])
    
    
    DAM_EMU_COMBO <- EMU_dam_code_test %>%
      
      ## EMU <- EMU_dam_code_test[2]
      lapply(function(EMU) {
        
        message('combine dam areas ', EMU)
        EMU_models <- dam_subset_sort[grepl(EMU, dam_subset_sort)] 
        
        ## only use these files :
        # *_final_mga56
        # *_hydropoints_off_SO3_above.
        # *_hydroareas_off_SO3_above_mga56_var_100_9
        
        ## Just join everything to the one feature layer
        dam_feat               <- dam_subset[[EMU_models[1]]] %>% 
          filter(LandUseFactor > 0)     %>% 
          st_transform(., st_crs(8058)) %>% st_make_valid() %>% 

          group_by(EXTRACTION_MANAGEMENT_UNIT) %>% 
          summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>%
          nngeo::st_remove_holes()
        
        plot(st_geometry(dam_feat))
      
        
      }) %>% bind_rows(.)
    
    

    gc()
    
    message('Writing Water Sources and Land Use aIntersect')
    st_write(DAM_EMU_COMBO,
             dsn    = NSW_CHR_WS_Intersections_database_loc,
             layer  = 'DAM_EMU_COMBO',
             quiet  = TRUE,
             append = TRUE)
    
    
    ## Stream Order
    EMU_Stream_Order   <- st_read(dsn   = paste0(catchment_data,
                                                 "NSW_Coastal_Streams.gpkg"),
                                  layer = 'NSW_Coastal_Streams') %>%
      st_transform(., st_crs(8058)) %>%
      st_crop(., NSW_CHR_Water_Sources_aggregated_test) %>%
      st_intersection(., NSW_CHR_Water_Sources_aggregated_test) %>%
      filter(., STRAHLER > 3)
    
    message('Writing Water Sources and Land Use aIntersect')
    st_write(EMU_Stream_Order,
             dsn        = NSW_CHR_WS_Intersections_database_loc,
             layer      = 'EMU_Stream_Order',
             quiet      = TRUE,
             append     = TRUE)
    
    
  }
  
}




#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################