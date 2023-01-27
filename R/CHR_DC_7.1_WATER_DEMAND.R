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


## CHR Zonal stat summaries
CHR_LGA_median <- read_csv(paste0(DC7_out, 'CHR_LGA_median.csv'))





# DC 7 :: TOWN WATER DEMAND ----


# Consequence rating based on the IRSD score at scale of SA1. Summarise into CHR water source, 
# THAN score of 1-5 obtained from a classification method for binning (20 percentiles)
NSW_CHR_WS_DC_7.1_TOWN_CONSEQ <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., select(CHR_LGA_median, 
                      CHR_Water_Source, 
                      Water_Demand_Ha, 
                      Water_Demand_Pop), 
            by = "CHR_Water_Source") %>% 
  
  ## Then the 20th percentile on this metric by CHR Water Source.
  mutate(DC_7.1_WATER_Ha  = ntile(Water_Demand_Ha,  5),
         DC_7.2_WATER_Pop = ntile(Water_Demand_Pop, 5)) %>% 
  
  ## Currently, NA are replaced with 0.
  ## Does this make sense?
  replace(is.na(.), 0) %>% 
  select(-CHR_Ha, -CHR_Sqkm) %>% 
  select(CHR_Water_Source, 
         Water_Demand_Ha, 
         DC_7.1_WATER_Ha, 
         Water_Demand_Pop, 
         DC_7.2_WATER_Pop)





## Save data ----
write_csv(NSW_CHR_WS_DC_7.1_TOWN_CONSEQ, 
          paste0(DC7_out, 
                 'NSW_CHR_WS_DC_7.1_TOWN_CONSEQ.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################