############################################################################################
###############################  ------ DC ALL ---- ########################################
############################################################################################


# \ 
# 
# This code combines all consequence ratings for all DCs
#
#   \


## ENVIRONMENT SETTINGS =============================================================


## To-do ----

## For 6.1 and 6.2, NA = 1
## Update the st_intersection with the additional arguments



## water source geography 
NSW_CHR_Water_Sources_aggregated <- 
  
  st_read(dsn = paste0(data_dir, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') 


NSW_CHR_Water_Sources_df <- NSW_CHR_Water_Sources_aggregated %>% 
  as_tibble() %>% select(-geom)


NSW_CHR_WS_EST_LUT <- 
  read_csv(paste0(data_dir, 
                  'NSW_CHR_Water_Sources_LUT.csv')) %>% 
  select(CHR_Water_Source, Est_No1) %>% distinct() %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')


## CHR Zonal stat summaries
CHR_LGA_median <- read_csv(paste0(DC8_out, 'CHR_LGA_median.csv'))
CHR_SA1_median <- read_csv(paste0(DC8_out, 'CHR_SA1_median.csv'))
CHR_POA_median <- read_csv(paste0(DC5_out, 'CHR_POA_median.csv'))




# 6.1 :: NUMBER OF TOURISM BUSINESSES ----


# Consequence rating based on the number of businesses per unit population size in reach 
# LGA (count divided by population size). Then rasterise. Then summarize into CHR water source.  
# Sum for all Water Sources within an Estuary Catchment. Score of 1-5 obtained for each Estuary 
# Catchment  from a classification method for binning (20 percentiles). 


NSW_CHR_WS_DC_6.1_TOURISM <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., select(CHR_LGA_median, 
                      CHR_Water_Source, 
                      Tour_Biz_Density), by = "CHR_Water_Source") %>% 
  
  left_join(., NSW_CHR_WS_EST_LUT, by = "CHR_Water_Source") %>%
  
  
  ## Save out the full table of GMs with each LUE class.
  {. ->> NSW_CHR_WS_EST_TOURISM_int_df } %>%
  
  group_by(Est_No1) %>% 
  summarise(Tour_Biz_Density = sum(Tour_Biz_Density)) %>% 
  
  ## Then 20 the percentile on this metric by CHR Water Source.
  mutate(DC_6.1_TOUR = ntile(Tour_Biz_Density, 5)) %>% 
  
  # replace(is.na(.), 0) %>% 
  select(Est_No1, 
         Tour_Biz_Density, 
         DC_6.1_TOUR) %>% 
  
  left_join(NSW_CHR_WS_EST_LUT, ., by = "Est_No1") %>% 
  group_by(CHR_Water_Source)                       %>% 
  summarise(Tour_Biz_Density = sum(Tour_Biz_Density),
            DC_6.1_TOUR      = max(DC_6.1_TOUR))




# 6.2 :: AQUACULTURE POTENTIAL ----


# Consequence rating assessed at each estuary using the Aquaculture_Leases layer. 
# Consequence rating according to area sum of leased land, divided by the area of 
# the estuary.  Score of 1-5 obtained from a classification method for binning (20 percentiles).



Aquaculture_leases <- st_read(dsn = paste0(economic_data, 
                                           'Aquaculture/Aquaculture_Leases.shp')) %>% 
  st_transform(., st_crs(8058))


NSW_estuaries <- st_read(paste0(DPEW_sayers_data, 
                                'Estuaries_Numbers11.shp')) %>% 
  
  st_transform(., st_crs(8058)) %>% 
  rename(Est_No1 = AltNumberi)  %>%
  select(Est_No1) %>% 
  
  ## Calculate the areas
  dplyr::mutate(m2       = st_area(.),
                Est_Ha   = units::set_units(m2, value = ha),
                Est_Ha   = drop_units(Est_Ha),
                
                Est_Sqkm = units::set_units(m2, value = km2),
                Est_Sqkm = drop_units(Est_Sqkm)) %>% 
  select(Est_No1, Est_Ha, Est_Sqkm)



## 
EST_AQUACUL_int_agg <- st_intersection(st_make_valid(NSW_estuaries),   
                                       st_make_valid(Aquaculture_leases)) %>%
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Non-overlapping water sources are 0
  mutate(m2               = st_area(.),
         Est_AQUACUL_Ha   = units::set_units(m2, value = ha),
         Est_AQUACUL_Ha   = drop_units(Est_AQUACUL_Ha)) %>% 
  
  # Consequence rating according to area sum of leased land, divided by the area of 
  # the estuary.  Score of 1-5 obtained from a classification method for binning (20 percentiles).
  
  ## Save out the full table of GMs with each LUE class.
  {. ->> EST_AQUACUL_int_sf } %>%
  as_tibble () %>% select(-m2)  %>% 
  {. ->> EST_AQUACUL_int_df } %>%
  
  group_by(Est_No1) %>%
  summarise(Est_Ha         = median(Est_Ha),
            Est_AQUACUL_Ha = sum(Est_AQUACUL_Ha)) %>% 
  
  mutate(Est_AQUACUL_percent  = (Est_AQUACUL_Ha/Est_Ha) * 100 %>% round(.),
         
         ## Consequence rating according to concentration of important habitat 
         ## (proportion of areas occupied by the value). 
         Est_AQUACUL_Score = ntile(Est_AQUACUL_percent, 5)) %>% 
  
  ## Get the columns
  dplyr::select(Est_No1, 
                Est_Ha, 
                Est_AQUACUL_Ha, 
                Est_AQUACUL_percent, 
                Est_AQUACUL_Score)


CHR_EST_AQUACUL_int_df <- EST_AQUACUL_int_agg %>% as_tibble() %>% 
  
  dplyr::select(Est_No1, 
                Est_AQUACUL_Ha, 
                Est_AQUACUL_percent, 
                Est_AQUACUL_Score) 



# Then join this value back on to the CHR_WS
CHR_WS_EST_AQUACUL_int_df <- NSW_CHR_WS_EST_LUT %>% 
  
  left_join(., select(CHR_EST_AQUACUL_int_df, 
                      Est_No1, 
                      Est_AQUACUL_percent, 
                      Est_AQUACUL_Score),
            by = "Est_No1")


NSW_CHR_WS_DC_6.2_AQUACULT <- CHR_WS_EST_AQUACUL_int_df %>% 
  na.omit() %>% 
  group_by(CHR_Water_Source) %>% 
  
  summarise(DC_6.2_AQUACULT     = max(Est_AQUACUL_Score),
            Est_AQUACUL_percent = sum(Est_AQUACUL_percent)) %>% 
  
  select(CHR_Water_Source, Est_AQUACUL_percent, DC_6.2_AQUACULT)


## Should NA stay NA, or be 0?
NSW_CHR_WS_DC_6_BUSINESS <- left_join(NSW_CHR_WS_DC_6.1_TOURISM,
                                      NSW_CHR_WS_DC_6.2_AQUACULT)



## Save data ----


message('writing EST-BIZ Intersect to geo-package')



st_write(EST_AQUACUL_int_sf,
         dsn    = NSW_CHR_WS_Intersections_database_loc,
         layer  = 'EST_AQUACUL_int_sf',
         quiet  = TRUE,
         append = TRUE)


st_write(EST_AQUACUL_int_agg,
         dsn    = NSW_CHR_WS_Intersections_database_loc,
         layer  = 'EST_AQUACUL_int',
         quiet  = TRUE,
         append = TRUE)



write_csv(NSW_CHR_WS_DC_6_BUSINESS, 
          paste0(DC6_out, 
                 'NSW_CHR_WS_DC_6_BUSINESS.csv'))






#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################