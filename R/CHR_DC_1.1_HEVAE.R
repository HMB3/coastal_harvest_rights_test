############################################################################################
####################################  ------ DC1.1 HEVAE ---- ##############################
############################################################################################


# \ 
# 
# This code analyzes the impact of extraction on the HEVAE
#
#   \


## ENVIRONMENT SETTINGS =============================================================


## To-do

## Re-format HEVAE Risk and Threatened species tables



## HEVAE Layers ----


## water source geography 
NSW_CHR_Water_Sources_aggregated <- 
  
  st_read(dsn = paste0(catchment_data, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>% 
  st_transform(., st_crs(8058))



NSW_CHR_Water_Sources_df <- 
  
  NSW_CHR_Water_Sources_aggregated %>% 
  as_tibble() %>% select(CHR_Water_Source)


NSW_HEVAE_layer <- 
  
  st_read(dsn   = paste0(eco_data, 
                         'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'HEVAE_NSW') %>% 
  
  st_transform(., st_crs(8058)) %>% 
  
  ## Calculate the areas
  dplyr::mutate(m2       = st_area(geom),
                HEV_Ha   = units::set_units(m2, value = ha),
                HEV_Ha   = drop_units(HEV_Ha),
                
                HEV_Sqkm = units::set_units(m2, value = km2),
                HEC_Sqkm = drop_units(HEV_Sqkm)) 


## Update
NSW_HEVAE_distinct_layer <- 
  
  st_read(dsn = paste0(eco_data, 
                       'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'HEVAE_Distinct') %>%
  
  st_transform(., st_crs(8058))     %>% 
  
  ## Calculate the areas
  dplyr::mutate(m2        = st_area(geom),
                HEVD_Ha   = units::set_units(m2, value = ha),
                HEVD_Ha   = drop_units(HEVD_Ha),
                
                HEVD_Sqkm = units::set_units(m2, value = km2),
                HEVD_Sqkm = drop_units(HEVD_Sqkm))





## Wetlands feature Layers ----
NSW_Coastal_Wetlands <- 
  
  st_read(dsn = paste0(eco_data, 
                       'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'NSW_Kingsford_Coastal_Wetlands') %>% 
  
  ## Calculate the areas
  dplyr::mutate(m2            = st_area(geom),
                CoastWet_Ha   = units::set_units(m2, value = ha),
                CoastWet_Ha   = drop_units(CoastWet_Ha),
                
                CoastWet_Sqkm = units::set_units(m2, value = km2),
                CoastWet_Sqkm = drop_units(CoastWet_Sqkm)) 


NSW_Important_Wetlands <- 
  
  st_read(dsn = paste0(eco_data, 
                       'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'NSW_Important_Coastal_Wetlands') %>% 
  
  ## Calculate the areas
  dplyr::mutate(m2          = st_area(geom),
                ImpWet_Ha   = units::set_units(m2, value = ha),
                ImpWet_Ha   = drop_units(ImpWet_Ha),
                
                ImpWet_Sqkm = units::set_units(m2, value = km2),
                ImpWet_Sqkm = drop_units(ImpWet_Sqkm))


NSW_RAMSAR_Wetlands <- 
  
  st_read(dsn = paste0(eco_data, 
                       'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'NSW_RAMSAR_Coastal_Wetlands')%>% 
  
  ## Calculate the areas
  dplyr::mutate(m2          = st_area(geom),
                RAMSAR_Ha   = units::set_units(m2, value = ha),
                RAMSAR_Ha   = drop_units(RAMSAR_Ha),
                
                RAMSAR_Sqkm = units::set_units(m2, value = km2),
                RAMSAR_Sqkm = drop_units(RAMSAR_Sqkm)) 





## We need to join several tables together from different feature layers,
## and also combine this with DPE LUTs (i.e. sources of truth).
plot(st_geometry(NSW_CHR_Water_Sources_aggregated));
plot(st_geometry(NSW_HEVAE_layer));
plot(st_geometry(NSW_Important_Wetlands))
names(NSW_CHR_Water_Sources_aggregated)



# Create Species columns? ----

## For each water source, we need a flag for :
## Does it contain any threatened species?
## If so, which ones?





## DC 1.1 :: HEVAE Values ----


CHR_HEAVE_MER_median <- read_csv(paste0(DC1_out, 
                                        'CHR_HEAVE_MER_median'))    %>% 
  
  mutate(DC_1.1_HEVAE = round(HEV_MER),
         DC_1.1_HEVAE = ifelse(DC_1.1_HEVAE == 0, 1, DC_1.1_HEVAE)) %>% 
  select(-HEV_MER) 



## That join needs to wait till we have the HEVAE data and species data organized properly
NSW_CHR_Water_Sources_DC_1.1_HEVAE_CONSQ <- NSW_CHR_Water_Sources_df %>%
  
  ## only 167 water sources have a HEVAE Rating...
  full_join(.,
            HEAVAE_consequence_combined_max,
            by = "CHR_Water_Source") %>% 
  
  # Need to make sure that we capture the difference between no data vs a 1
  # for wetlands...if there is no wetlands in your join with a water source, 
  # then it gets a 1, not missing data
  # so for Wetlands (measure 1.2) there should be a value in all water sources
  mutate(DC_1.1_HEVAE = pmax(`zero flow periods`, 
                             `low flows`, 
                             `freshes`, 
                             `1.5ARI`, 
                             `2.5ARI`)) %>% na.omit() %>% 
  bind_rows(CHR_HEAVE_MER_median)


## Join on the MER values




## Need the HEVAE and species tables to do that, need tidier data from John to do that.
## Meanwhile put the steps in place



## DC 1.2 :: Wetlands Values ----


## The following method is adapted from the MER (PC 1.2)
## The goal is to estimate the % of each water source that is covered by
## particular ecological features, eg wetlands.

## This should be done as the % area that the raster cells of the eco-features cover
## in the water source feature layer.


# 1. For each of the wetland feature layers (Ramsar, NSW Important Wetlands and NSW Wetlands), add a field 
# in the attribute table called ‘Raster’ and assign all cells a value of ‘1’
NSW_Coastal_Wetlands <- NSW_Coastal_Wetlands %>% 
  
  ## Create the new field
  dplyr::mutate(Coast_Wetlands_Raster = 1)


NSW_Coastal_Wetlands_agg <- NSW_Coastal_Wetlands %>% 
  
  ## group by CHR_Water_Source,  don't care about the others
  group_by(Coast_Wetlands_Raster) %>% 
  
  ## This creates slivers, which need to be removed...
  summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
  nngeo::st_remove_holes()


NSW_Important_Wetlands <- NSW_Important_Wetlands %>% 
  
  ## Create the 'new' field
  dplyr::mutate(Import_Wetlands_Raster = 1)


NSW_Important_Wetlands_agg <- NSW_Important_Wetlands %>% 
  
  ## group by CHR_Water_Source,  don't care about the others
  group_by(Import_Wetlands_Raster) %>% 
  
  ## This creates slivers, which need to be removed...
  summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
  # st_snap(x = ., y = ., tolerance = 0.0001) %>% 
  nngeo::st_remove_holes()


NSW_RAMSAR_Wetlands <- NSW_RAMSAR_Wetlands %>% 
  
  ## Create the new field
  dplyr::mutate(RAMSAR_Wetlands_Raster = 1)


NSW_RAMSAR_Wetlands_agg <- NSW_RAMSAR_Wetlands %>% 
  
  ## group by CHR_Water_Source,  don't care about the others
  group_by(RAMSAR_Wetlands_Raster) %>% 
  
  ## This creates slivers, which need to be removed...
  summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
  nngeo::st_remove_holes()



## Wetlands Rasters ----
# 2. Convert each feature layers to raster, based on the ‘Raster’ field. Select a cell size which allows 
# the most accurate data representation, while considering the computational effort required for the conversion. 
# The smaller the cell the more memory required of the computer. We can use 250m grid cells from CSIRO
NSW_Coastal_Wetlands_raster <- fasterize::fasterize(NSW_Coastal_Wetlands_agg, 
                                                    csiro.climate.grids.250m[["Annual_mean_temp"]],
                                                    field = "Coast_Wetlands_Raster")
names(NSW_Coastal_Wetlands_raster) <- c('Coast_Wetlands_Raster')


NSW_Important_Wetlands_raster <- fasterize::fasterize(NSW_Important_Wetlands_agg, 
                                                      csiro.climate.grids.250m[["Annual_mean_temp"]],
                                                      field = "Import_Wetlands_Raster")
names(NSW_Important_Wetlands_raster) <- c('Import_Wetlands_Raster')


NSW_RAMSAR_Wetlands_raster <- fasterize::fasterize(NSW_RAMSAR_Wetlands_agg, 
                                                   csiro.climate.grids.250m[["Annual_mean_temp"]],
                                                   field = "RAMSAR_Wetlands_Raster")
names(NSW_RAMSAR_Wetlands_raster) <- c('RAMSAR_Wetlands_Raster')


# 3. Reclassify each raster output so that 1 = 1 and Null = 0. 
NSW_Coastal_Wetlands_raster[is.na(NSW_Coastal_Wetlands_raster[])]     <- 0 
NSW_Important_Wetlands_raster[is.na(NSW_Important_Wetlands_raster[])] <- 0 
NSW_RAMSAR_Wetlands_raster[is.na(NSW_RAMSAR_Wetlands_raster[])]       <- 0


## These raster need to be re-coded. So it's a two-step process.
## Re-code the rasters, and/or calculate the area.
## For the Ramsar and NSW Important Wetlands, every cell == 5
## But what about the Wetlands? Are they 1 too?
NSW_Important_Wetlands_raster[NSW_Important_Wetlands_raster == 1] <- 5 
NSW_RAMSAR_Wetlands_raster[NSW_RAMSAR_Wetlands_raster == 1]       <- 5


## Create a raster stack
NSW_wetlands_rasters <- raster::stack(NSW_Coastal_Wetlands_raster,
                                      NSW_Important_Wetlands_raster,
                                      NSW_RAMSAR_Wetlands_raster)


## Create a mosaic of all the raster wetlands
NSW_wetlands_raster_combo_score <- terra::mosaic(NSW_Coastal_Wetlands_raster,
                                                 NSW_Important_Wetlands_raster,
                                                 NSW_RAMSAR_Wetlands_raster,
                                                 fun = "max")


## All these rasters should be saved out for safe keeping



## Intersect the water sources with the wetlands
message('Intersecting Water Sources with the NSW Wetlands')


## Wetlands Join  ----
# Listing of state, national or international significance Score 
# The presence of either an Important wetland, OR a Ramsar wetland, the score == 5
# = >40% of grid cells  in the water source contain a wetland, the score == 4
# = >30% of grid cells  in the water source contain a wetland  the score == 3
# = >20% of grids cells in water source contain a wetland, the score ==  2
# 0 <=> 20% of grids cells in water source contain a wetland, the score ==  1
# 0% of grids in water source contain a wetland, the score == 0
Water_source_NSW_wetlands_int <- 
  
  ## Update 
  st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                  st_make_valid(NSW_Coastal_Wetlands_agg)) %>%
  
  ## Join on Water sources that don't have Wetlands
  full_join(., NSW_CHR_Water_Sources_df, 
            by = c("CHR_Water_Source")) %>% 
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Under the score column, apply the criteria based on the results of 
  ## the ‘% of Water Source’
  mutate(m2                 = st_area(geom),
         WS_NSW_Wet_Ha      = units::set_units(m2, value = ha),
         WS_NSW_Wet_Ha      = drop_units(WS_NSW_Wet_Ha),
         
         WS_NSW_Wet_Sqkm    = units::set_units(m2, value = km2),
         WS_NSW_Wet_Sqkm    = drop_units(WS_NSW_Wet_Sqkm),
         
         WS_NSW_Wet_percent = (WS_NSW_Wet_Ha/CHR_Ha) * 100 %>% round(.),
         
         ## Classify the wetlands score
         Wetlands_Score     = case_when(WS_NSW_Wet_percent >= 40 ~ 5,
                                        WS_NSW_Wet_percent >= 30  & WS_NSW_Wet_percent < 40 ~ 4,
                                        WS_NSW_Wet_percent >= 20  & WS_NSW_Wet_percent < 30 ~ 3,
                                        WS_NSW_Wet_percent < 20   & WS_NSW_Wet_percent >= 1 ~ 2,
                                        WS_NSW_Wet_percent < 1  ~ 1,
                                        TRUE ~ as.numeric(WS_NSW_Wet_percent))) %>%
  
  # Need to makes sure that we capture the difference between no data vs a 1
  # for wetlands...if there is no wetlands in your join with a water source, 
  # then it gets a 1, not missing data
  # so for Wetlands (measure 1.2) there should be a value in all water sources
  mutate(WS_NSW_Wet_percent = ifelse(is.na(WS_NSW_Wet_percent), 0, WS_NSW_Wet_percent),
         Wetlands_Score     = ifelse(is.na(Wetlands_Score),     1, Wetlands_Score)) %>% 
  
  
  dplyr::select(CHR_Water_Source, 
                WS_NSW_Wet_Ha, 
                WS_NSW_Wet_Sqkm,
                WS_NSW_Wet_Ha, 
                WS_NSW_Wet_percent, 
                Wetlands_Score, 
                geom)


Water_source_NSW_wetlands_int_df <- Water_source_NSW_wetlands_int %>% 
  
  as_tibble() %>% 
  dplyr::select(CHR_Water_Source, 
                WS_NSW_Wet_Ha, 
                WS_NSW_Wet_percent, 
                Wetlands_Score) 



## Intersect the water sources with the Important wetlands
Water_source_Import_wetlands_int <- 
  
  st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                  st_make_valid(NSW_Important_Wetlands_agg)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Non-overlapping water sources are 0
  mutate(m2                  = st_area(geom),
         WS_IMP_Wet_Ha       = units::set_units(m2, value = ha),
         WS_IMP_Wet_Ha       = drop_units(WS_IMP_Wet_Ha),
         
         WS_IMP_Wet_Sqkm     = units::set_units(m2, value = km2),
         WS_IMP_Wet_Sqkm     = drop_units(WS_IMP_Wet_Sqkm),
         
         WS_IMP_Wet_percent  = (WS_IMP_Wet_Ha/CHR_Ha) * 100 %>% round(.),
         
         # 5. For the Ramsar and NSW Important Wetlands, add a column called ‘Score’, 
         ## and apply a 5 to all cells.
         Import_Wetlands_Score = 5) %>% 
  
  dplyr::select(CHR_Water_Source, 
                WS_IMP_Wet_Ha, 
                WS_IMP_Wet_Sqkm,
                WS_IMP_Wet_Ha, 
                WS_IMP_Wet_percent, 
                Import_Wetlands_Score, 
                geom)


Water_source_Import_wetlands_int_df <- Water_source_Import_wetlands_int %>% as_tibble() %>% 
  
  dplyr::select(CHR_Water_Source, 
                WS_IMP_Wet_Ha, 
                WS_IMP_Wet_percent, 
                Import_Wetlands_Score) 


## Intersect the water sources with the RAMSAR wetlands
Water_source_RAMSAR_wetlands_int <- 
  
  st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                  st_make_valid(NSW_RAMSAR_Wetlands_agg)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Non-overlapping water sources are 0
  mutate(m2               = st_area(geom),
         WS_RAMS_Ha       = units::set_units(m2, value = ha),
         WS_RAMS_Ha       = drop_units(WS_RAMS_Ha),
         WS_RAMS_Sqkm     = units::set_units(m2, value = km2),
         WS_RAMS_Sqkm     = drop_units(WS_RAMS_Sqkm),
         
         WS_RAMS_percent  = (WS_RAMS_Ha/CHR_Ha) * 100 %>% round(.),
         
         # 5. For the Ramsar and NSW Important Wetlands, add a column called ‘Score’, 
         ## and apply a 5 to all cells.
         RAMSAR_Wetlands_Score = 5) %>% 
  
  dplyr::select(CHR_Water_Source, 
                WS_RAMS_Ha, 
                WS_RAMS_Sqkm,
                WS_RAMS_Ha, 
                WS_RAMS_percent, 
                RAMSAR_Wetlands_Score, 
                geom)


Water_source_RAMSAR_wetlands_int_df <- Water_source_RAMSAR_wetlands_int %>% 
  
  as_tibble() %>% 
  dplyr::select(CHR_Water_Source, 
                WS_RAMS_Ha, 
                WS_RAMS_percent, 
                RAMSAR_Wetlands_Score) 




## Join the water source 1.2 score data back on to the water source table.
NSW_Water_Source_1.2_Consequence <- NSW_CHR_Water_Sources_aggregated %>% 
  
  ## Join measures 1.2 on 
  list(Water_source_NSW_wetlands_int_df,
       Water_source_Import_wetlands_int_df,
       Water_source_RAMSAR_wetlands_int_df) %>% 
  
  ## We need the water sources that didn't intersect
  reduce(full_join, 
         by = c("CHR_Water_Source")) %>% 
  
  replace(., is.na(.), 0) %>% 
  
  dplyr::select(CHR_Water_Source,
                
                WS_NSW_Wet_Ha, 
                WS_IMP_Wet_Ha, 
                WS_RAMS_Ha,
                WS_NSW_Wet_percent,  
                WS_IMP_Wet_percent,  
                WS_RAMS_percent,
                Wetlands_Score, 
                Import_Wetlands_Score, 
                RAMSAR_Wetlands_Score, 
                
                geom) 


NSW_Water_Source_1.2_max_Consequence_feat <- NSW_Water_Source_1.2_Consequence %>% 
  
  as_tibble() %>% 
  select(CHR_Water_Source, 
         WS_NSW_Wet_percent, 
         Wetlands_Score, 
         WS_IMP_Wet_percent,
         Import_Wetlands_Score,
         WS_RAMS_percent,
         RAMSAR_Wetlands_Score) %>% 
  
  mutate(DC_1.2_WET = pmax(`Wetlands_Score`, 
                           `Import_Wetlands_Score`, 
                           `RAMSAR_Wetlands_Score`)) 


## 
NSW_Water_Source_DC_1.2_max_wetlands <- 
  
  ##
  NSW_Water_Source_1.2_max_Consequence_feat %>% 
  
  select(CHR_Water_Source, 
         WS_NSW_Wet_percent, 
         WS_IMP_Wet_percent,
         WS_RAMS_percent,
         DC_1.2_WET) %>% 
  
  group_by(CHR_Water_Source) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE) 





## Save Data -----
write_csv(NSW_CHR_Water_Sources_DC_1.1_HEVAE_CONSQ, 
          paste0(DC1_out, 'NSW_CHR_WS_DC_1.1_HEVAE_CONSQ.csv'))


write_csv(NSW_Water_Source_DC_1.2_max_wetlands, 
          paste0(DC1_out, 'NSW_CHR_WS_DC_1.2_WETLANDS_CONSQ.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################