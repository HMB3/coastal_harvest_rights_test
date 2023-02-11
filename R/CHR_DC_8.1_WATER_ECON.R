############################################################################################
###############################  ------ DC 8 ---- ##########################################
############################################################################################


# \ 
# 
# This code calculates consequence ratings for the Water-dependent measures of Economic Measures
#
#   \


## ENVIRONMENT SETTINGS =============================================================


## To-do ----


## For 8.1 and 8.2, NA = ?
## Update the st_intersection with the additional arguments



## Re-format ABS data to be programmatic
LUE_intersect <- FALSE



## Water Source geographies
NSW_CHR_Water_Sources_aggregated <- 
  
  ## Update
  st_read(dsn = DPEW_Water_Source_database_loc,
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


NSW_CHR_DAM_AREAS <- read_csv(paste0(hydro_out, 'dams/', 'NSW_CHR_WS_Farm_Dam_AGG_Areas.csv')) %>% 
  select(CHR_Water_Source, Dam_Sqkm, Volume_ML) 


## LGA stats
CHR_Context_Tables     <- read_excel_allsheets(paste0(economic_data, 'CHR_DPEW_Context_Input_Data.xlsx'))
WS_ABS_SA1_combo_index <- CHR_Context_Tables$WS_ABS_SA1_combo_index
NSW_LGA_Water_Combo    <- CHR_Context_Tables$NSW_LGA_Water_Combo
Gross_margins_values   <- CHR_Context_Tables$Gross_margins_values


## CHR Zonal stat summaries
CHR_LGA_median <- read_csv(paste0(DC8_out, 'CHR_LGA_median.csv'))
CHR_SA1_median <- read_csv(paste0(DC8_out, 'CHR_SA1_median.csv'))




# plot(st_geometry(NSW_LUE_coastal))



# 8.1 :: AVERAGE ANNUAL FARM WATER USE ----


## Data :
## Annual licensed water-use per hectare (DPE data) in CSV format by LGA
## Consequence rating according to Use/hectare. Rasterize proportion (LGA).
## Zonal stats on this raster. Then 20 the percentile on this metric by CHR Water Source.
NSW_CHR_WS_DC_8.1_WATER_ECON_Consequence <- NSW_CHR_Water_Sources_df %>% 
  
  left_join(., select(CHR_LGA_median, CHR_Water_Source, Water_use_Ha),        by = "CHR_Water_Source") %>%
  left_join(., select(CHR_SA1_median, CHR_Water_Source, Agriculture_percent), by = "CHR_Water_Source") %>% 
  
  ## Then 20 the percentile on this metric by CHR Water Source.
  mutate(Water_Use_Score            = ntile(Water_use_Ha,     5),
         Agriculture_percent_Score  = ntile(Agriculture_percent, 5)) %>% 
  
  # replace(is.na(.), 0) %>% 
  
  rename(DC_8.1_WATER_USE = Water_Use_Score,
         DC_8.3_AG_EMP    = Agriculture_percent_Score)




# 8.2 :: AGRICULTURAL GROSS MARGINS ----


## Intersect the water sources with the wetlands
message('Intersecting Water Sources with the NSW Land use Geogpraphy')


# Consequence (opportunities) according to gross margin multiplied by 
# area of land in LUE categories with irrigation infrastructure (only theses 
# categories have GM values. Land use from NSW DPE, Gross margins from DPI.
NSW_CHR_WS_LUE_int <- 
  
  st_read(dsn     = NSW_CHR_WS_Intersections_database_loc,
          layer   = 'NSW_CHR_WS_LUE_int') %>%
  st_transform(., st_crs(8058))






NSW_CHR_WS_Aggregated_LUE_GM_Scores <- NSW_CHR_WS_LUE_int %>% 
  
  ## Calculate the areas of intersect - prob unnecessary.
  select(-LUE_Ha, -LUE_Sqkm) %>% 
  mutate(m2           = st_area(geom),
         CHR_LUE_Ha   = units::set_units(m2, value = ha), 
         CHR_LUE_Ha   = drop_units(CHR_LUE_Ha),
         
         CHR_LUE_Sqkm = units::set_units(m2, value = km2), 
         CHR_LUE_Sqkm = drop_units(CHR_LUE_Sqkm)) %>%
  
  # replace(is.na(.), 0)   %>% 
  rename(Low_GM  = Low,
         Mid_GM  = Mid,
         High_GM = high) %>% 
  
  ## combine margin and area to calculate 
  ## likelihood on change in %
  
  # Consequence (opportunities) according to gross margin multiplied by 
  # area of land in LUE categories with irrigation infrastructure.
  # GM units are $ per ML. 
  mutate(Gross_Margin_Low_Area   = CHR_LUE_Sqkm * Low_GM  %>% round(.),
         Gross_Margin_Mid_Area   = CHR_LUE_Sqkm * Mid_GM  %>% round(.),
         Gross_Margin_High_Area  = CHR_LUE_Sqkm * High_GM %>% round(.)) %>% 
  
  ## Save out the full table of GMs with each LUE class.
  {. ->> NSW_CHR_WS_LUE_Full_GM_Areas_sf } %>%
  as_tibble () %>% select(-geom, -m2)      %>% 
  {. ->> NSW_CHR_WS_LUE_Full_GM_Areas_df } %>%
  
  ## Sum the area multiplication by water source, remove LUE category
  ## group_by CHR, then divide by the area of land eligible for Farm dams (Roper),
  filter(Gross_Margin_Low_Area > 0) %>% 
  group_by(CHR_Water_Source) %>% 
  
  ## Map of of contours / Vol *100
  left_join(., NSW_CHR_DAM_AREAS, by = 'CHR_Water_Source') %>% 
  summarise(CHR_Sqkm     = median(CHR_Sqkm),
            Dam_Sqkm     = median(Dam_Sqkm),
            Volume_ML    = median(Volume_ML),
            CHR_LUE_Sqkm = sum(CHR_LUE_Sqkm),
            
            Low_GM                     = median(Low_GM),
            Gross_Margin_Low_Dam_Area  = sum(Gross_Margin_Low_Area)  / Dam_Sqkm,
            
            Mid_GM                     = median(Mid_GM),
            Gross_Margin_Mid_Dam_Area  = sum(Gross_Margin_Mid_Area)  / Dam_Sqkm,
            
            High_GM                    = median(High_GM),
            Gross_Margin_High_Dam_Area = sum(Gross_Margin_High_Area) / Dam_Sqkm) %>%
  
  ## This gives us the value of land that can be used to generate revenue
  ## Score of 1-5 obtained from a classification method for binning (20 percentiles)?
  mutate(Gross_Margin_Low_Score  = ntile(Gross_Margin_Low_Dam_Area,  5),
         Gross_Margin_Mid_Score  = ntile(Gross_Margin_Mid_Dam_Area,  5), 
         Gross_Margin_High_Score = ntile(Gross_Margin_High_Dam_Area, 5)) %>% 
  
  ## SELECT
  dplyr::select(CHR_Water_Source, 
                CHR_Sqkm, 
                CHR_LUE_Sqkm,
                Dam_Sqkm,
                
                Low_GM,
                Gross_Margin_Low_Dam_Area,
                Gross_Margin_Low_Score,
                
                Mid_GM,
                Gross_Margin_Mid_Dam_Area,
                Gross_Margin_Mid_Score,
                
                High_GM,
                Gross_Margin_High_Dam_Area,
                Gross_Margin_High_Score) 



## Now just get the Mid score, and re-name to DC8.2 
NSW_CHR_WS_DC_8_WATER_ECON_CONSEQ <- NSW_CHR_WS_Aggregated_LUE_GM_Scores %>%
  
  ## Select cols
  rename(DC_8.2_GROSS = Gross_Margin_Mid_Score) %>% 
  full_join(., NSW_CHR_WS_DC_8.1_WATER_ECON_Consequence, by = "CHR_Water_Source") %>% 
  
  dplyr::select(CHR_Water_Source, 
                CHR_LUE_Sqkm,
                
                Water_use_Ha,
                DC_8.1_WATER_USE,
                
                Gross_Margin_Mid_Dam_Area,
                DC_8.2_GROSS,
                
                Agriculture_percent,
                DC_8.3_AG_EMP) 





## Save out table and spatial data ----
write_csv(NSW_CHR_WS_LUE_Full_GM_Areas_df, 
          paste0(DC8_out, 
                 'Water_Source_LUE_Full_GM_Areas_df.csv'))


write_csv(NSW_CHR_WS_Aggregated_LUE_GM_Scores, 
          paste0(DC8_out, 
                 'Water_Source_Aggregated_LUE_GM_Scores.csv'))


write_csv(NSW_CHR_WS_DC_8_WATER_ECON_CONSEQ, 
          paste0(DC8_out, 
                 'NSW_CHR_WS_DC_8.2_GROSS_CONSEQ.csv'))




message('writing LUE-CHR Intersect to geo-package')
st_write(NSW_CHR_WS_LUE_Full_GM_Areas_sf,
         dsn    = NSW_CHR_WS_Intersections_database_loc,
         layer  = 'Water_source_NSW_LUE_Full_GM_Areas',
         quiet  = TRUE,
         append = TRUE)





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################

