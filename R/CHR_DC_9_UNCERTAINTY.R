############################################################################################
###############################  ------ DC 5.1 ---- ########################################
############################################################################################


# \ 
# 
# This code calculates consequence ratings for the Socio-Economic Measures
#
#   \


## ENVIRONMENT SETTINGS =============================================================


## To-do

## Re-format ABS data to be programmatic



## water source geography 
NSW_unreg_coastal_water_sources_aggregated <- 
  
  st_read(dsn = paste0(data_dir, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_unreg_coastal_water_sources_aggregated') %>% 
  
  ## Calculate the areas
  dplyr::mutate(Hectares  = st_area(geom)/hectare_conversion,
                Hectares  = drop_units(Hectares),
                Sq_km     = st_area(geom)/km_conversion,
                Sq_km     = drop_units(Sq_km)) %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, 
                W_Source_ID,
                Hectares,
                Sq_km,
                geom)



# ABS GEOGRPAHY ----


## Read in the Project feature data here from the inputs geo-database
ABS_CEN2021_LGA_coastal <- 
  
  st_read(dsn = paste0(economic_dir, 
                       '/ABS/ABS_G01_LGA_AUST_GDA2020.gpkg'),
          layer = 'ABS_G01_LGA_AUST_GDA2020') %>% 
  
  st_transform(., st_crs(8058)) %>% 
  
  ## Calculate the areas
  dplyr::mutate(LGA_Hectares  = st_area(geom)/hectare_conversion,
                LGA_Hectares  = drop_units(LGA_Hectares),
                LGA_Sq_km     = st_area(geom)/km_conversion,
                LGA_Sq_km     = drop_units(LGA_Sq_km)) %>% 
  
  dplyr::select(id, LGA_CODE_2021,
                LGA_NAME_2021,
                LGA_Hectares, LGA_Sq_km,
                everything(),
                geom) %>% 
  
  .[NSW_unreg_coastal_water_sources_aggregated, ]



ABS_CEN2021_POA_coastal <- 
  
  st_read(dsn = paste0(economic_dir, 
                       '/ABS/ABS_G01_POA_AUST_GDA2020.gpkg'),
          layer = 'ABS_G01_POA_AUST_GDA2020') %>% 
  
  st_transform(., st_crs(8058)) %>% 
  
  ## Calculate the areas
  dplyr::mutate(POA_Hectares  = st_area(geom)/hectare_conversion,
                POA_Hectares  = drop_units(POA_Hectares),
                POA_Sq_km     = st_area(geom)/km_conversion,
                POA_Sq_km     = drop_units(POA_Sq_km)) %>% 
  
  dplyr::select(id, POA_CODE_2021,
                POA_NAME_2021,
                POA_Hectares, POA_Sq_km, AREA_ALBERS_SQKM,
                everything(),
                geom) %>% 
  
  .[NSW_unreg_coastal_water_sources_aggregated, ]


ABS_LGA_POA_coastal_LUT <-   
  
  st_intersection(st_make_valid(dplyr::select(ABS_CEN2021_LGA_coastal, LGA_CODE_2021, LGA_NAME_2021)),   
                  st_make_valid(dplyr::select(ABS_CEN2021_POA_coastal, POA_CODE_2021, POA_NAME_2021))) %>% 
  
  st_intersection(., st_make_valid(NSW_unreg_coastal_water_sources_aggregated)) %>% 
  as_tibble() %>% dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID,
                                LGA_NAME_2021, POA_NAME_2021) %>% distinct()


## SEIFA Indexes
SEIFA_LGA_Indexes <- read_excel_allsheets(paste0(economic_data,
                                             '/ABS/SEIFA_LGA_indexes.xls'))


SEIFA_SA1_Indexes <- read_excel_allsheets(paste0(economic_data,
                                                 '/ABS/SEIFA_SA1_indexes.xls'))



## John's estuary layer
# The field name 'Est_No1' in CoastalWaterSourcesMetroMgtZones links with the field name 
# "AltNumberi" in the Estuary feature





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
NSW_CAPAD_raster <- fasterize::fasterize(NSW_CAPAD_Marine_estuary, 
                                         raster_template_250m,
                                         field = "CAPAD_Raster")
names(NSW_CAPAD_raster) <- c('CAPAD_Raster')



# 3. Reclassify each raster output so that 1 = 1 and Null = 0. 
NSW_CAPAD_raster[is.na(NSW_CAPAD_raster[])]         <- 0 



## These rasters themselves may need to be re-coded, based on their intersection with the WS
## So it's a two-step process.
## Re-code the rasters, and/or calculate the area.
# NSW_Important_Wetlands_raster[NSW_Important_Wetlands_raster == 1] <- 5 
# NSW_RAMSAR_Wetlands_raster[NSW_RAMSAR_Wetlands_raster == 1]       <- 5



# 4. Run Zonal statistics using the Water Sources feature layer (Unregulated) 
## on all wetland rasters. But what function should this be? Median, mean, sum, etc?
## I don' think this is the raster count that we want.
## For each, we need - the area of the water source, and the are covered by the wetlands
## Count of cells 
# zonal_count <- terra::extract(NSW_wetlands_rasters, 
#                               NSW_unreg_coastal_water_sources_aggregated,
#                               fun   = median, 
#                               na.rm = TRUE) %>% as_tibble()





##  INTERSECTIONS ---- 


## Intersect the water sources with the wetlands
message('Intersecting Water Sources with the ABS Geogpraphy')

## This definition is taken from the MER 
# Listing of state, national or international significance Score 
# The presence of either an Important wetland, OR a Ramsar wetland, the score == 5
# = >40% of grid cells  in the water source contain a wetland, the score == 4
# = >30% of grid cells  in the water source contain a wetland  the score == 3
# = >20% of grids cells in water source contain a wetland, the score ==  2
# 0 <=> 20% of grids cells in water source contain a wetland, the score ==  1
# 0% of grids in water source contain a wetland, the score == 0


Water_source_NSW_CAPAD_int <- 
  
  st_intersection(st_make_valid(NSW_unreg_coastal_water_sources_aggregated),   
                  st_make_valid(NSW_CAPAD_Marine_estuary)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Under the score column, apply the criteria based on the results of 
  ## the ‘% of Water Source’
  mutate(Hectares_WS_CAPAD = st_area(geom)/hectare_conversion,
         Hectares_WS_CAPAD = drop_units(Hectares_WS_CAPAD),
         WS_CAPAD_percent  = (Hectares_WS_CAPAD/Hectares) * 100 %>% round(.),
         
         ## Classify the CAPAD score
         CAPAD_Score = case_when(WS_CAPAD_percent >= 40 ~ 5,
                                 WS_CAPAD_percent >= 30  & WS_CAPAD_percent < 40 ~ 4,
                                 WS_CAPAD_percent >= 20  & WS_CAPAD_percent < 30 ~ 3,
                                 WS_CAPAD_percent < 20   & WS_CAPAD_percent > 3  ~ 2,
                                 WS_CAPAD_percent < 3    & WS_CAPAD_percent > 1  ~ 1,
                                 WS_CAPAD_percent < 1    ~ 0,
                                 TRUE ~ as.numeric(WS_CAPAD_percent))) %>%
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, Hectares, Sq_km,
                Hectares_WS_CAPAD, WS_CAPAD_percent, CAPAD_Score, geom)


Water_source_NSW_CAPAD_int_df <- Water_source_NSW_CAPAD_int %>% as_tibble() %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, Hectares_WS_CAPAD, 
                WS_CAPAD_percent, CAPAD_Score) 



## Intersect the water sources with the Important wetlands
Water_source_macrophytes_int <- 
  
  st_intersection(st_make_valid(NSW_unreg_coastal_water_sources_aggregated),   
                  st_make_valid(NSW_MACROPH_estuary)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Non-overlapping water sources are 0
  mutate(Hectares_WS_MACRO = st_area(geom)/hectare_conversion,
         Hectares_WS_MACRO = drop_units(Hectares_WS_MACRO),
         WS_MACRO_percent  = (Hectares_WS_MACRO/Hectares) * 100 %>% round(.),
         
         # 5. For the Ramsar and NSW Important Wetlands, add a column called ‘Score’, 
         ## and apply a 5 to all cells.
         WS_MACRO_Score = case_when(WS_MACRO_percent >= 40 ~ 5,
                                    WS_MACRO_percent >= 30 & WS_MACRO_percent < 40 ~ 4,
                                    WS_MACRO_percent >= 20 & WS_MACRO_percent < 30 ~ 3,
                                    WS_MACRO_percent < 20  & WS_MACRO_percent > 3  ~ 2,
                                    WS_MACRO_percent < 3   & WS_MACRO_percent > 1  ~ 1,
                                    WS_MACRO_percent < 1   ~ 0,
                                    TRUE ~ as.numeric(WS_MACRO_percent))) %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, Hectares, Sq_km,
                Hectares_WS_MACRO, WS_MACRO_percent, WS_MACRO_Score, geom)


Water_source_macrophytes_int_df <- Water_source_macrophytes_int %>% as_tibble() %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, Hectares_WS_MACRO, 
                WS_MACRO_percent, WS_MACRO_Score) 


## Intersect the water sources with the RAMSAR wetlands
Water_source_blue_carbon_int <- 
  
  st_intersection(st_make_valid(NSW_unreg_coastal_water_sources_aggregated),   
                  st_make_valid(NSW_BLUE_CARB_agg)) %>%
  
  ## Get the % of each Water source covered by _any_ of the wetlands. 
  ## Non-overlapping water sources are 0
  mutate(Hectares_WS_CARB = st_area(geom)/hectare_conversion,
         Hectares_WS_CARB = drop_units(Hectares_WS_CARB),
         WS_CARB_percent  = (Hectares_WS_CARB/Hectares)*100 %>% round(.),
         
         # 5. For the CARB and NSW Important Wetlands, add a column called ‘Score’, 
         ## and apply a 5 to all cells.
         WS_CARBON_Score = case_when(WS_CARB_percent >= 40 ~ 5,
                                     WS_CARB_percent >= 30 & WS_CARB_percent < 40 ~ 4,
                                     WS_CARB_percent >= 20 & WS_CARB_percent < 30 ~ 3,
                                     WS_CARB_percent < 20  & WS_CARB_percent > 3  ~ 2,
                                     WS_CARB_percent < 3   & WS_CARB_percent > 1  ~ 1,
                                     WS_CARB_percent < 1  ~ 0,
                                     TRUE ~ as.numeric(WS_CARB_percent))) %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, Hectares, Sq_km,
                Hectares_WS_CARB, WS_CARB_percent, WS_CARBON_Score, geom)


Water_source_blue_carbon_int_df <- Water_source_blue_carbon_int %>% as_tibble() %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, 
                W_Source_ID, 
                Hectares_WS_CARB, 
                WS_CARB_percent, 
                WS_CARBON_Score) 





## Join the water source 1.2 score data back on to the water source table.
NSW_Water_Source_Eco_Consequence <- NSW_unreg_coastal_water_sources_aggregated %>% 
  
  ## Join measures 1.2 on 
  list(Water_source_NSW_wetlands_int_df,
       Water_source_Import_wetlands_int_df,
       Water_source_RAMSAR_wetlands_int_df) %>% 
  
  ## We need the water sources that didn't intersect
  reduce(full_join, 
         by = c("UNREGULATED_WATER_SOURCE", "W_Source_ID")) %>% 
  
  dplyr::select(UNREGULATED_WATER_SOURCE, W_Source_ID, Hectares, Sq_km,
                
                Hectares_WS_NSW_Wet, Hectares_WS_IMP_Wet, Hectares_WS_RAM_Wet,
                WS_NSW_Wet_percent,  WS_IMP_Wet_percent,  WS_RAM_Wet_percent,
                
                Wetlands_Score, Import_Wetlands_Score, RAMSAR_Wetlands_Score, geom) %>% 
  
  replace(is.na(.), 0)



## Clean up the data
NSW_Water_Source_Consequence_df <- NSW_Water_Source_Consequence %>% 
  as.data.frame() %>% dplyr::select(-geom)


write_csv(NSW_Water_Source_Consequence_df, 
          paste0(enviro_out, 'CHR_Water_Source_DC_1.3_Wetlands_Conseq_Rating.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################