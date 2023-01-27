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
  select(CHR_Water_Source, Est_No1) %>% distinct()


## CHR Zonal stat summaries
CHR_LGA_median <- read_csv(paste0(DC8_out, 'CHR_LGA_median.csv'))
CHR_SA1_median <- read_csv(paste0(DC8_out, 'CHR_SA1_median.csv'))
CHR_POA_median <- read_csv(paste0(DC5_out, 'CHR_POA_median.csv'))




# 5.1 :: MEAURE OF RELATIVE DIS-ADVANTANTAGE ----


# Consequence rating based on the IRSD score at scale of SA1. Summarise into CHR water source, 
# THAN score of 1-5 obtained from a classification method for binning (20 percentiles)
NSW_CHR_WS_DC_5.1_SEIFA <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., select(CHR_SA1_median, CHR_Water_Source, 
                      NSW_Percentile, Unemployed_percent), by = "CHR_Water_Source") %>% 
  
  ## Then 20 the percentile on this metric by CHR Water Source.
  mutate(DC_5.1_SEIFA  = ntile(NSW_Percentile,     5),
         DC_5.2_UNEMP  = ntile(Unemployed_percent, 5)) %>% 
  
  # replace(is.na(.), 0) %>% 
  select(-CHR_Ha, -CHR_Sqkm) %>% 
  select(CHR_Water_Source, NSW_Percentile, DC_5.1_SEIFA, Unemployed_percent, DC_5.2_UNEMP)




# 5.5 :: IMPORTANCE OF BOATING ----


# Consequence rating based on average annual number of licenses within each Estuary Catchment. 
# To get number of licenses first calculate density (license per unit areas of postcode), then 
# rastrsie the density, then zonal stats with CHR Water Source (mean), 


NSW_CHR_WS_DC_5_MARITIME <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., select(CHR_POA_median, CHR_Water_Source, 
                      Fishing_density, Boat_density), by = "CHR_Water_Source") %>% 
  
  #  then multiply by water  source area to get number of license in water source.  
  # Sum for all Water Sources within an Estuary Catchment. Score of 1-5 obtained 
  # for each Estuary Catchment from a classification method for binning (20 percentiles)
  mutate(Fishing_density_Area = CHR_Sqkm * Fishing_density,
         Boat_density_Area    = CHR_Sqkm * Boat_density) %>% 
  
  left_join(., NSW_CHR_WS_EST_LUT, by = "CHR_Water_Source") %>%
  
  
  ## Save out the full table of GMs with each LUE class.
  {. ->> NSW_CHR_WS_EST_MARITIME_int_df } %>%

  group_by(Est_No1) %>% 
  summarise(Fishing_density_Area_Est = sum(Fishing_density_Area),
            Boat_density_Area_Est    = sum(Boat_density_Area)) %>% 
  
  ## Then 20 the percentile on this metric by CHR Water Source.
  mutate(DC_5.5_FISH = ntile(Fishing_density_Area_Est, 5),
         DC_5.6_BOAT = ntile(Boat_density_Area_Est,    5)) %>% 
  
  # replace(is.na(.), 0) %>% 
  select(Est_No1, 
         Fishing_density_Area_Est, 
         DC_5.5_FISH, 
         Boat_density_Area_Est, 
         DC_5.6_BOAT) %>% 
  
  left_join(NSW_CHR_WS_EST_LUT, ., by = "Est_No1") %>% 
  group_by(CHR_Water_Source) %>% 
  summarise(Fishing_density_Area_Est = sum(Fishing_density_Area_Est),
            DC_5.5_FISH              = max(DC_5.5_FISH),
            
            Boat_density_Area_Est    = sum(Boat_density_Area_Est),
            DC_5.6_BOAT              = max(DC_5.6_BOAT))





## Save data ----
write_csv(NSW_CHR_WS_DC_5.1_SEIFA, 
          paste0(DC5_out, 
                 'NSW_CHR_WS_DC_5.1_SEIFA.csv'))


write_csv(NSW_CHR_WS_EST_MARITIME_int_df, 
          paste0(DC5_out, 
                 'NSW_CHR_WS_EST_MARITIME_int.csv'))


write_csv(NSW_CHR_WS_DC_5_MARITIME, 
          paste0(DC5_out, 
                 'NSW_CHR_WS_DC_5_MARITIME.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################