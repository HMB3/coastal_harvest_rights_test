############################################################################################
###############################  ------ DC ALL ---- ########################################
############################################################################################


# \ 
# 
# This code combines all consequence ratings for all DCs
#
#   \


## To-do

## 



## LUT DATA =============================================================


## CHR Boundaries
dim(NSW_CHR_Water_Sources_aggregated)
dim(NSW_CHR_WS_test)


## CHR WS Master LUT
NSW_CHR_WS_LUT <- read_excel_allsheets(paste0(catchment_data, 'CHR_WS_NUM_HEVAE_LUT_Data.xlsx'))
NSW_CHR_WS_FULL_LUT <- NSW_CHR_WS_LUT$NSW_CHR_WS_FULL_LUT
NSW_CHR_WS_EST_LUT  <- NSW_CHR_WS_FULL_LUT %>% select(CHR_Water_Source, Est_No1) %>% distinct()
NSW_CHR_WS_EMU_LUT  <- NSW_CHR_WS_FULL_LUT %>% select(CHR_Water_Source, EXTRACTION_MANAGEMENT_UNIT) %>% distinct()
NSW_CHR_WS_WSP_LUT  <- NSW_CHR_WS_FULL_LUT %>% select(CHR_Water_Source, WATER_SHARING_PLAN)         %>% distinct()


## Environmental profiles ----
NSW_CHR_WS_PROFILE      <- read_csv(paste0(enviro_out, 'NSW_CHR_Water_Sources_enviro_profile.csv')) 
NSW_CHR_EMU_PROFILE     <- read_csv(paste0(enviro_out, 'NSW_CHR_EMU_PROFILE.csv')) %>% na.omit()
NSW_CHR_WS_HYDRO_INPUTS <- read_csv(paste0(hydro_out,  'NSW_CHR_WS_HYDRO_INPUTS_df.csv')) %>% 
  na.omit() %>% select(-CHR_Ha, -CHR_Sqkm)
NSW_CHR_EMU_AREA        <- NSW_CHR_EMU_PROFILE %>% select(EXTRACTION_MANAGEMENT_UNIT, EMU_Sqkm)


## Open geopackage
NSW_CHR_EMU_ <-
  
  st_read(dsn = DPEW_Water_Source_database_loc,
          layer = 'NSW_CHR_EMU_aggregated') %>%
  
  dplyr::mutate(m2        = st_area(geom),
                EMU_Ha   = units::set_units(m2, value = ha),
                EMU_Ha   = drop_units(EMU_Ha),
                
                EMU_Sqkm = units::set_units(m2, value = km2),
                EMU_Sqkm = drop_units(EMU_Sqkm)) %>% 
  
  as_tibble() %>% 
  select(EXTRACTION_MANAGEMENT_UNIT, EMU_Ha, EMU_Sqkm)
  
  


## LAND USE
## This will include a classification of the LUEs into Ag, etc.
## only the ones they are interested in...
NSW_CHR_WS_LUE_GM_LUT <-
  
  st_read(dsn = NSW_CHR_WS_Intersections_database_loc,
          layer = 'Water_source_NSW_LUE_Full_GM_Areas') %>% 
  
  as_tibble() %>% select(-geom) %>% 
  left_join(., NSW_CHR_WS_EMU_LUT, by = "CHR_Water_Source") %>% 
  left_join(., NSW_CHR_EMU_AREA,   by = "EXTRACTION_MANAGEMENT_UNIT") %>% 
  select(EXTRACTION_MANAGEMENT_UNIT, CHR_Water_Source, everything())





# COMBINE MEASURES ----


## List of all the layers
consequence_list <- list.files(conseq_out, 
                               pattern    = 'NSW_CHR_WS_DC', 
                               full.names = TRUE,
                               recursive  = TRUE) %>% .[-13]


## Read in the Project feature data here from the inputs geo-database
Consequence_combined_table <- consequence_list %>%
  
  ## Pipe the list into lapply
  ## table <- consequence_list[1]
  lapply(function(table) {
    
    message('Combine consequence ratings ', table)
    
    table <- 
      read_csv(table) %>%
      filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>%
      select(., -one_of(c('CHR_Ha, CHR_Sqkm'))) %>% 
      select(., -one_of(c('CHR_Ha.x, CHR_Sqkm.y"')))
    
    
  }) %>% reduce(., full_join, 
                by = c("CHR_Water_Source")) %>% as_tibble() %>% 
  
  select(CHR_Water_Source,
         
         "1.5ARI",
         "2.5ARI",
         "zero flow periods",
         "freshes",
         DC_1.1_HEVAE,
         
         WS_IMP_Wet_percent,
         WS_NSW_Wet_percent,
         WS_RAMS_percent,
         DC_1.2_WET,
         DC_1.3_MACRO,
         DC_1.4_CARBON,
         DC_1.7_CAPAD,
         
         DC_2.2_AV_EXTR_LOW,
         DC_2.2_AV_EXTR_HIGH,
         
         EST_RISK,
         DC_3.1_EST_HEALTH,
         DC_3.2_ALGAL_low,
         DC_3.2_ALGAL_high,
         
         NTT_percent,
         DC_4.1_NTT,
         AIMS_density,
         DC_4.2_AIMS,
         
         NSW_Percentile,
         DC_5.1_SEIFA,
         Unemployed_percent,
         DC_5.2_UNEMP,
         Fishing_density_Area_Est,
         DC_5.5_FISH,
         Boat_density_Area_Est,
         DC_5.6_BOAT,
         
         Tour_Biz_Density,
         DC_6.1_TOUR,
         Est_AQUACUL_percent,
         DC_6.2_AQUACULT,
         
         Water_Demand_Ha,
         DC_7.1_WATER_Ha,
         Water_Demand_Pop,
         DC_7.2_WATER_Pop,
         
         Water_use_Ha,
         DC_8.1_WATER_USE,
         Gross_Margin_Mid_Dam_Area,
         DC_8.2_GROSS,
         Agriculture_percent,
         DC_8.3_AG_EMP)


## Count the NA values and cross-check
NA_1.1 <- Consequence_combined_table %>% filter(is.na(DC_1.1_HEVAE))
intersect(NA_1.1$CHR_Water_Source, HEAVAE_consequence_all$CHR_Water_Source)




# JOIN MEASURES TO CHR ----


##  
NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf <- NSW_CHR_Water_Sources_aggregated %>% 
  
  left_join(., Consequence_combined_table, by = "CHR_Water_Source")


NSW_CHR_WS_DC_CONSEQUENCE_SCORES_df <- NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf %>% 
  
  as_tibble() %>% select(-geom)





## Save data ----
write_csv(NSW_CHR_WS_DC_CONSEQUENCE_SCORES_df, 
          paste0(conseq_out, 
                 'NSW_CHR_WS_DC_CONSEQUENCE_SCORES_df.csv'))


## We want to remove the old layers to update the calculations
if(file.exists(NSW_CHR_WS_CONSEQ_Layers_database_loc)) {
  
  file.remove(NSW_CHR_WS_CONSEQ_Layers_database_loc)
  
}


message('writing LUE-CHR Intersect to geo-package')
st_write(NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf,
         dsn        = NSW_CHR_WS_CONSEQ_Layers_database_loc,
         layer      = 'NSW_CHR_WS_DC_CONSEQUENCE_SCORES',
         quiet      = TRUE,
         append     = TRUE,
         delete_dsn = TRUE)





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
