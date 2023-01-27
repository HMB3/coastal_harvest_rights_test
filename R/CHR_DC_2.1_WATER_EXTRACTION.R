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
CHR_GROUNDW_median <- read_csv(paste0(DC2_out, 'CHR_GROUNDW_median.csv'))






# DC 2 :: AVERAGE EXTRACTION ----


# Consequence rating based on the IRSD score at scale of SA1. Summarise into CHR water source, 
# THAN score of 1-5 obtained from a classification method for binning (20 percentiles)
NSW_CHR_WS_DC_2.2_AV_EXTRACT <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., CHR_GROUNDW_median, 
            by = "CHR_Water_Source") %>% 
  
  ## Then the 20th percentile on this metric by CHR Water Source.
  rename(DC_2.2_AV_EXTR_LOW  = M_3_1_scor,
         DC_2.2_AV_EXTR_HIGH = M_12_2_sco) %>% 
  
  select(-CHR_Ha, -CHR_Sqkm) %>% 
  select(CHR_Water_Source, DC_2.2_AV_EXTR_LOW, DC_2.2_AV_EXTR_HIGH)




## Save data ----
write_csv(NSW_CHR_WS_DC_2.2_AV_EXTRACT, 
          paste0(DC2_out, 
                 'NSW_CHR_WS_DC_2.2_AV_EXTRACT.csv'))





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################