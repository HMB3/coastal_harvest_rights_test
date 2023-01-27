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

## 



## water source geography 
NSW_CHR_Water_Sources_aggregated <- 
  
  st_read(dsn = paste0(data_dir, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') 


NSW_CHR_Water_Sources_df <- NSW_CHR_Water_Sources_aggregated %>% 
  as_tibble() %>% select(-geom)



NSW_CHR_Algal_Blooms_int <- 
  
  st_read(dsn = paste0(eco_data, 'CHR_NSW_Input_Ecological_Layers_Data.gpkg'),
          layer = 'NSW_CHR_Algal_Blooms_int')  


## CHR Zonal stat summaries
CHR_EST_median <- read_csv(paste0(DC3_out, 'CHR_EST_median.csv')) 




# DC 3.1 :: CURRENT ESTUARY HEALTH RISK ----


# Consequence rating based on the IRSD score at scale of SA1. Summarise into CHR water source, 
# THAN score of 1-5 obtained from a classification method for binning (20 percentiles)
NSW_CHR_WS_DC_3.1_EST_HEALTH <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., select(CHR_EST_median,  CHR_Water_Source, 
                      EST_RISK), 
            by = "CHR_Water_Source") %>% 
  
  ## Then the 20th percentile on this metric by CHR Water Source.
  mutate(DC_3.1_EST_HEALTH = ntile(EST_RISK, 5)) %>% 
  
  # replace(is.na(.), 0) %>% 
  select(-CHR_Ha, -CHR_Sqkm) %>% 
  select(CHR_Water_Source, EST_RISK, DC_3.1_EST_HEALTH)




# DC 3.2 :: ALGAL BLOOMS ----


## 
NSW_CHR_WS_DC_3.2_ALGAL <- NSW_CHR_Algal_Blooms_int %>% 
  as_tibble() %>% select(-geom) %>% 
  
  group_by(CHR_Water_Source) %>% 
  summarise(DC_3.2_ALGAL_low  = max(M_4_2_scor)  %>% ceiling(),
            DC_3.2_ALGAL_high = max(M_12_2_sco)) 


NSW_CHR_WS_DC_3_WATER_QUALITY <- left_join(NSW_CHR_WS_DC_3.1_EST_HEALTH,
                                           NSW_CHR_WS_DC_3.2_ALGAL, 
                                           by = "CHR_Water_Source") %>% 
  
  ## Everything that is NA for Estuary health, is assigned a 1.
  replace(is.na(.), 1) 



## Save data ----
write_csv(NSW_CHR_WS_DC_3_WATER_QUALITY , 
          paste0(DC3_out, 
                 'NSW_CHR_WS_DC_3_WATER_QUALITY.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################