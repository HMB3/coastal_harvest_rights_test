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



# 4 :: Context Raster data ----


## Intersections ----
if(conseq_inter) {
  
  
  
  ## Read in water source geographies
  NSW_CHR_Water_Sources_aggregated <- 
    
    ## Update
    st_read(dsn = paste0(catchment_data, 
                         'CHR_DPEW_Input_Water_Source_Data.gpkg'),
            layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
    filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>% 
    st_transform(., st_crs(8058))
  
  
  
  
  
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
  
  
  ## DC 8.1 ----
  
  
  if(GM) {
    
    ## Update
    NSW_2ND_LUE_GM <- st_read(dsn = NSW_Context_Layers_database_loc,
                              layer = 'NSW_2ND_LUE_GM')
    
    message('Intersecting Water Sources and Land Use')
    NSW_CHR_WS_LUE_int <- 
      
      st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                      st_make_valid(NSW_2ND_LUE_GM)) 
    
    
    message('Writing Water Sources and Land Use aIntersect')
    st_write(NSW_CHR_WS_LUE_int,
             dsn        = NSW_CHR_WS_Intersections_database_loc,
             layer      = 'NSW_CHR_WS_LUE_int',
             quiet      = TRUE,
             append     = TRUE,
             delete_dsn = TRUE)
    
  }
  
  
}




#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################