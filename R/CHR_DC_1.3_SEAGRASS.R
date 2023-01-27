############################################################################################
###############################  ------ DC 1.3 ---- ########################################
############################################################################################


# \ 
# 
# This code calculates consequence ratings for the Sea Grasses and Estuaries
#
#   \


## ENVIRONMENT SETTINGS =============================================================


## To-do :

## Re-format HEVAE Risk and Threatened species tables
## Check flow metrics
WS_AGG <- FALSE




# SEA GRASS FEATURES ----


## Read in water source geographies
NSW_CHR_Water_Sources_aggregated <- 
  
  ## Update
  st_read(dsn = paste0(catchment_data, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>% 
  st_transform(., st_crs(8058))



NSW_CHR_Water_Sources_df <- NSW_CHR_Water_Sources_aggregated %>% 
  as_tibble() %>% select(-geom)



## CHR LUT
NSW_CHR_WS_EST_LUT <- 
  read_csv(paste0(data_dir, 
                  'NSW_CHR_Water_Sources_LUT.csv')) %>% 
  select(CHR_Water_Source, Est_No1) %>% distinct()



## John's estuary layer
# The field name 'Est_No1' in CoastalWaterSourcesMetroMgtZones links with 
# the field name "AltNumberi" in the Estuary feature.


## Note that the estuaries don't have a big overlap with the ecological layers
## It may not makes sense to clip to them for all layers, we may need to buffer them.
## "Within_WS" links it back to water source.
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


NSW_estuaries_df <- NSW_estuaries %>% as_tibble() %>% select(-geometry) 




NSW_CHR_estuaries <-   st_read(dsn = paste0(catchment_data, 
                                            'CHR_DPEW_Input_Water_Source_Data.gpkg'),
                               layer = 'NSW_EST_Num_aggregated') %>% 
  st_transform(., st_crs(8058))



## Bring in the protected areas 
NSW_CAPAD_Marine <- 
  
  st_read(dsn = paste0(eco_data, 
                       'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'NSW_CAPAD_Marine')  %>%    
  
  dplyr::mutate(CAPAD_Raster = 1) %>% 
  group_by(CAPAD_Raster) %>% 
  
  summarize() %>% st_make_valid() %>% 
  st_buffer(., 0.0)        %>% 
  nngeo::st_remove_holes() %>% 
  
  dplyr::select(CAPAD_Raster, 
                geometry)


# 1. For each of the Reserves, Estuaries and Macrophytes, add a field 
# in the attribute table called ‘Raster’ and assign all cells a value of ‘1’
# Not sure what we do with the different categories.
# EG do they all get a 1, then it just comes down to % covered?
NSW_CAPAD_Marine_estuary <- st_read(dsn = paste0(eco_data, 
                                                 'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
                                    layer = 'NSW_CAPAD_Marine_estuary')


## Macro-phytes - sea grasses and mangroves 
NSW_MACROPH <- st_read(dsn = paste0(data_dir, 
                                    'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
                       layer = 'NSW_MACROPH') %>% 
  filter(!is.na(MACROPHYTE)) %>% 
  filter(AREA_M2_Z5 > 0) %>% 
  mutate(MACROPHYTE_Raster = 1)


## Macrophytes - sea grasses and mangroves.
## This doesn't overlap that much...doesn't make sense to crop it
NSW_BLUE_CARB <- st_read(dsn = paste0(eco_data, 
                                      'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
                         layer = 'NSW_BLUE_CARB') %>%
  
  ## This might not be needed
  # dplyr::rename(BLUE_CARB_Raster = gridcode) %>% 
  dplyr::select(BLUE_CARB_Raster, geom) 


NSW_BLUE_CARB_agg <- 
  
  st_read(dsn = paste0(eco_data, 
                       'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'NSW_BLUE_CARB_agg')


## Plot data
# plot(st_geometry(NSW_estuaries));
# plot(st_geometry(NSW_CAPAD_Marine_estuary));
# plot(st_geometry(NSW_MACROPH));
# plot(st_geometry(NSW_BLUE_CARB_agg))





# 1.3 :: SEA GRASS RASTERS ----


## The following method is adapted from the MER (PC 1.2)
## The goal is to estimate the % of each water source that is covered by
## particular ecological features, eg wetlands.


## This should be done as the % area that the raster cells of the eco-features cover
## in the water source feature layer.



# 2. Convert each feature layers to raster, based on the ‘Raster’ field. Select 
# a cell size which allows the most accurate data representation, while considering 
# the computational effort required for the conversion. The smaller the cell the 
# more memory required of the computer. We can use 250m grid cells from CSIRO
NSW_CAPAD_raster <- fasterize::fasterize(NSW_CAPAD_Marine, 
                                         raster_template_250m,
                                         field = "CAPAD_Raster")
names(NSW_CAPAD_raster)   <- c('CAPAD_Raster')


NSW_MACROPH_raster        <- fasterize::fasterize(NSW_MACROPH, 
                                                  raster_template_250m,
                                                  field = "MACROPHYTE_Raster")
names(NSW_MACROPH_raster) <- c('Macrophytes_Raster')


# NSW_MACROPH_TYPE_raster   <- fasterize::fasterize(NSW_MACROPH, 
#                                                   raster_template_250m,
#                                                   field = "MACROPHYTE")
# names(NSW_MACROPH_TYPE_raster) <- c('Macrophytes_TYPE_Raster')


NSW_BLUE_CARB_raster <- fasterize::fasterize(NSW_BLUE_CARB %>% st_cast(), 
                                             raster_template_250m,
                                             field = "BLUE_CARB_Raster")
names(NSW_BLUE_CARB_raster) <- c('Blue_Carbon_Raster')


# 3. Reclassify each raster output so that 1 = 1 and Null = 0. 
NSW_CAPAD_raster[is.na(NSW_CAPAD_raster[])]         <- 0 
NSW_MACROPH_raster[is.na(NSW_MACROPH_raster[])]     <- 0 
NSW_BLUE_CARB_raster[is.na(NSW_BLUE_CARB_raster[])] <- 0


## These rasters themselves may need to be re-coded, based on their intersection with the WS
## So it's a two-step process.
## Re-code the rasters, and/or calculate the area.
# NSW_Important_Wetlands_raster[NSW_Important_Wetlands_raster == 1] <- 5 
# NSW_RAMSAR_Wetlands_raster[NSW_RAMSAR_Wetlands_raster == 1]       <- 5


## Create a raster stack
NSW_MACROPH_rasters <- raster::stack(NSW_CAPAD_raster,
                                     NSW_MACROPH_raster,
                                     NSW_BLUE_CARB_raster)



# 4. Run Zonal statistics using the Water Sources feature layer (Unregulated) 
## on all wetland rasters. But what function should this be? Median, mean, sum, etc?
## I don' think this is the raster count that we want.
## For each, we need - the area of the water source, and the are covered by the wetlands
## Count of cells 
DC_Wetlands_Zonal <- terra::extract(NSW_MACROPH_rasters,
                                    NSW_CHR_Water_Sources_aggregated,
                                    fun   = median,
                                    na.rm = TRUE) %>% as_tibble()





##  INTERSECTIONS ---- 


## Intersect the water sources with the wetlands
message('Intersecting Water Sources with the NSW Wetlands')


## This definition is taken from the MER 
# Listing of state, national or international significance Score 
# The presence of either an Important wetland, OR a Ramsar wetland, the score == 5
# = >40% of grid cells  in the water source contain a wetland, the score == 5
# = >30% of grid cells  in the water source contain a wetland  the score == 3
# = >20% of grids cells in water source contain a wetland, the score ==  2
# 0 <=> 20% of grids cells in water source contain a wetland, the score ==  1
# 0% of grids in water source contain a wetland, the score == 0


## CAPAD ----
Water_source_NSW_CAPAD_int <- 
  
  st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                  st_make_valid(NSW_CAPAD_Marine)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Under the score column, apply the criteria based on the results of 
  ## the ‘% of Water Source’
  mutate(m2                = st_area(geom),
         WS_CAPAD_Ha       = units::set_units(m2, value = ha),
         WS_CAPAD_Ha       = drop_units(WS_CAPAD_Ha),
         WS_CAPAD_percent  = (WS_CAPAD_Ha/CHR_Ha) * 100 %>% round(.),
         
         ## Classify the CAPAD score
         CAPAD_Score = case_when(WS_CAPAD_percent >= 40 ~ 5,
                                 WS_CAPAD_percent >= 30  & WS_CAPAD_percent < 40 ~ 4,
                                 WS_CAPAD_percent >= 20  & WS_CAPAD_percent < 30 ~ 3,
                                 WS_CAPAD_percent < 20   & WS_CAPAD_percent >= 1 ~ 2,
                                 WS_CAPAD_percent < 1  ~ 1,
                                 TRUE ~ as.numeric(WS_CAPAD_percent))) %>%
  
  dplyr::select(CHR_Water_Source, 
                CHR_Ha, 
                CHR_Sqkm,
                WS_CAPAD_Ha, 
                WS_CAPAD_percent, 
                CAPAD_Score, 
                geom)


CHR_WS_CAPAD_df <- Water_source_NSW_CAPAD_int %>% as_tibble() %>% 
  
  dplyr::select(CHR_Water_Source, 
                WS_CAPAD_Ha, 
                WS_CAPAD_percent, 
                CAPAD_Score) 





## MACROPHYTES ----
## This measure is calculated for each Estuary, then joined back to the 
## CHR table using John's Estuary Number



EST_macrophytes_int <- 
  
  st_read(dsn   = NSW_CHR_WS_Intersections_database_loc,
          layer = 'ESTUARY_macrophytes_int') %>%
  st_transform(., st_crs(8058)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Non-overlapping water sources are 0
  mutate(m2            = st_area(geom),
         Est_MACRO_Ha  = units::set_units(m2, value = ha),
         Est_MACRO_Ha  = drop_units(Est_MACRO_Ha)) %>% 
  
  left_join(., NSW_estuaries_df, by = 'Est_No1') %>% 
  
  {. ->> EST_Num_macrophytes_int_sf } %>% 
  
  ## check this Group by
  group_by(Est_No1) %>%
  summarise(Est_Ha         = median(Est_Ha),
            Est_MACRO_Ha   = sum(Est_MACRO_Ha)) %>% 
  
  mutate(Est_MACRO_percent = (Est_MACRO_Ha/Est_Ha) * 100 %>% round(.),
         
         ## Consequence rating according to concentration of important habitat 
         ## (proportion of areas occupied by the value).
         ## Don't do this here, do it in the main consequence file
         Est_MACRO_Score = case_when(Est_MACRO_percent  >= 40 ~ 5,
                                     Est_MACRO_percent  >= 30 & Est_MACRO_percent < 40 ~ 4,
                                     Est_MACRO_percent  >= 20 & Est_MACRO_percent < 30 ~ 3,
                                     Est_MACRO_percent  < 20  & Est_MACRO_percent >= 1 ~ 2,
                                     Est_MACRO_percent  < 3   & Est_MACRO_percent > 1  ~ 1,
                                     Est_MACRO_percent  < 1   ~ 1,
                                     TRUE ~ as.numeric(Est_MACRO_percent))) %>% 
  
  ## Get the columns
  dplyr::select(Est_No1, 
                Est_Ha, 
                Est_MACRO_Ha, 
                Est_MACRO_percent, 
                Est_MACRO_Score, 
                geom)



CHR_EST_macrophytes_int_df <- EST_macrophytes_int %>% as_tibble() %>% 
  
  dplyr::select(Est_No1, 
                Est_MACRO_Ha, 
                Est_MACRO_percent, 
                Est_MACRO_Score) 


## Then join this value back on to the CHR_WS
CHR_WS_EST_macrophytes_int_df <- NSW_CHR_WS_EST_LUT %>% 
  
  left_join(., select(CHR_EST_macrophytes_int_df, 
                      Est_No1, 
                      Est_MACRO_percent, 
                      Est_MACRO_Score),
            
            by = "Est_No1")



CHR_WS_macrophytes_df <- CHR_WS_EST_macrophytes_int_df %>% 
  na.omit() %>% 
  group_by(CHR_Water_Source) %>% 
  summarise(Est_MACRO_Score   = max(Est_MACRO_Score),
            Est_MACRO_percent = median(Est_MACRO_percent))





## BLUE CARBON ESTUARY ----
EST_blue_carbon_int <- st_read(dsn   = NSW_CHR_WS_Intersections_database_loc,
          layer = 'ESTUARY_blue_carbon_int') %>%
  st_transform(., st_crs(8058))              %>% 

  ## Slight difference in the calc - sum of the CARBON scores/area of the 'estuary',
  ## THEN, do the n-tile on the area (always 5, which is the 20% percentile)
  
  ## Get the % of each Water source covered by _any_ of the wetlands. 
  ## Non-overlapping water sources are 0
  mutate(m2            = st_area(geom),
         Est_CARB_Ha   = units::set_units(m2, value = ha), 
         Est_CARB_Ha   = drop_units(Est_CARB_Ha),
         Est_BLUE_CARB = BLUE_CARB_Raster) %>% 
  
  left_join(., NSW_estuaries_df, by = 'Est_No1') %>% 
  
  {. ->> EST_Num_BlueCarb_int_sf } %>% 
  
  group_by(Est_No1) %>%
  summarise(Est_Ha        = median(Est_Ha),
            
            ## This must be the sum of the blue carbon value for the estuary intersect
            Est_CARB_Ha   = sum(Est_CARB_Ha),
            Est_BLUE_CARB = sum(Est_BLUE_CARB)) %>% 
  
  ## Consequence rating according to sum carbon storage potential divided by the area of the estuary.  
  ## Score of 1-5 obtained from a classification method for binning (20 percentiles).
  mutate(Est_CARB_area    = (Est_BLUE_CARB/Est_Ha)   %>% round(., 3),
         Est_CARBON_Score = ntile(Est_CARB_area, 5)) %>% 
  
  ## SELECT
  dplyr::select(Est_No1, 
                Est_Ha,
                Est_CARB_Ha, 
                Est_BLUE_CARB,
                Est_CARB_area, 
                Est_CARBON_Score, 
                geom)


EST_source_blue_carbon_int_df <- 
  
  EST_blue_carbon_int %>% as_tibble() %>% 
  
  dplyr::select(Est_No1, 
                Est_Ha,
                Est_CARB_Ha, 
                Est_BLUE_CARB,
                Est_CARB_area, 
                Est_CARBON_Score)


## Now join the Est_No1 back onto the water source back onto the 
CHR_WS_EST_CARBON_int_df <- NSW_CHR_WS_EST_LUT %>% 
  
  left_join(., select(EST_source_blue_carbon_int_df, 
                      Est_No1, 
                      Est_BLUE_CARB,
                      Est_CARB_area,
                      Est_CARBON_Score),
            by = "Est_No1")


CHR_WS_CARBON_df <- CHR_WS_EST_CARBON_int_df %>%
  
  na.omit()                  %>% 
  group_by(CHR_Water_Source) %>% 
  summarise(Est_BLUE_CARB    = median(Est_BLUE_CARB),
            Est_CARB_area    = max(Est_CARB_area),
            Est_CARBON_Score = max(Est_CARBON_Score))



## BLUE CARBON WS ----
if(WS_AGG) {
  
  Water_source_blue_carbon_int <- st_read(dsn   = NSW_CHR_WS_Intersections_database_loc,
                                          layer = 'NSW_CHR_WS_blue_carbon_int') %>%
    st_transform(., st_crs(8058)) %>% 
    
    ## Slight difference in the calc - sum of the CARBON scores/area of the 'estuary',
    ## THEN, do the n-tile on the area (always 5, which is the 20% percentile)
    
    ## Get the % of each Water source covered by _any_ of the wetlands. 
    ## Non-overlapping water sources are 0
    mutate(m2         = st_area(geom),
           WS_CARB_Ha = units::set_units(m2, value = ha), 
           WS_CARB_Ha = drop_units(WS_CARB_Ha)) %>% 
    
    group_by(CHR_Water_Source) %>%
    summarise(CHR_Ha     = median(CHR_Ha),
              WS_CARB_Ha = sum(WS_CARB_Ha),
              WS_CARB    = sum(CHR_Ha)) %>% 
    
    mutate(WS_CARB_percent = (WS_CARB/CHR_Ha)*100 %>% round(.),
           
           # 5. For the CARB and NSW Important Wetlands, add a column called ‘Score’, 
           ## and apply a 5 to all cells.
           WS_CARBON_Score = ntile(WS_CARB_percent, 5)) %>% 
    
    ## SELECT
    dplyr::select(CHR_Water_Source, 
                  CHR_Ha,
                  WS_CARB_Ha, 
                  WS_CARB_percent, 
                  WS_CARBON_Score, 
                  geom)
  
  
  Water_source_blue_carbon_int_df <- 
    
    Water_source_blue_carbon_int %>% as_tibble() %>% 
    
    dplyr::select(CHR_Water_Source, 
                  CHR_Ha,
                  WS_CARB_Ha, 
                  WS_CARB_percent, 
                  WS_CARBON_Score)
  
}




## COMBINE MEASURES ----
NSW_CHR_WS_DC_1.3_ESTAURIES_CONSQ <- 
  
  ## Aggregate the data at WS level - always start with CHR LUT
  NSW_CHR_Water_Sources_df %>% 
  
  ## Join measures 1.2 on 
  list(CHR_WS_CAPAD_df,
       CHR_WS_macrophytes_df,
       CHR_WS_CARBON_df) %>% 
  
  ## We need the water sources that didn't intersect
  reduce(full_join, 
         by = c("CHR_Water_Source")) %>% 
  
  ## How did this creep in?
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>% 
  
  dplyr::select(CHR_Water_Source,
                WS_CAPAD_percent,
                CAPAD_Score,
                Est_MACRO_percent,
                Est_MACRO_Score,
                Est_BLUE_CARB,
                Est_CARB_area,
                Est_CARBON_Score) %>% 
  
  rename(DC_1.7_CAPAD  = CAPAD_Score,
         DC_1.3_MACRO  = Est_MACRO_Score,
         DC_1.4_CARBON = Est_CARBON_Score)


## Save data ----
message('writing LUE-CHR Intersect to geo-package')
st_write(EST_Num_macrophytes_int_sf,
         dsn    = NSW_CHR_WS_Intersections_database_loc,
         layer  = 'EST_Num_macrophytes_int_sf',
         quiet  = TRUE,
         append = TRUE,
         delete_dsn = TRUE)


st_write(EST_Num_BlueCarb_int_sf,
         dsn    = NSW_CHR_WS_Intersections_database_loc,
         layer  = 'EST_Num_BlueCarb_int_sf',
         quiet  = TRUE,
         append = TRUE,
         delete_dsn = TRUE)



EST_Num_macrophytes_int_df <- as_tibble(EST_Num_macrophytes_int_sf) %>% select(-geom)

write_csv(EST_Num_macrophytes_int_df, 
          paste0(DC1_out, 'EST_Num_macrophytes_int_df.csv'))


EST_Num_BlueCarb_int_df <- as_tibble(EST_Num_BlueCarb_int_sf) %>% select(-geom)

write_csv(EST_Num_BlueCarb_int_df, 
          paste0(DC1_out, 'EST_Num_BlueCarb_int_df.csv'))



write_csv(NSW_CHR_WS_DC_1.3_ESTAURIES_CONSQ, 
          paste0(DC1_out, 'NSW_CHR_WS_DC_1.3_ESTAURIES_CONSQ.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################