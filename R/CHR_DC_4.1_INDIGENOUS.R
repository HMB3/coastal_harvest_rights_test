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


message(length(unique(NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)), ' MOS coastal WSs')


## Stop the code if there are 
stopifnot(length(unique(NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)) == 482)


# NATIVE TITLE LAYERS ----


## Native title data
NSW_NTT_DET <- 
  
  st_read(dsn = NSW_Context_Layers_database_loc,
          layer = 'NSW_NTT_DET') %>% 
  st_transform(., st_crs(8058)) 

NSW_NTT_DET_NA <- NSW_NTT_DET %>% filter(is.na(detoutcome))


plot(st_geometry(NSW_NTT_DET %>% filter(!is.na(detoutcome))), border = 'red')
plot(st_geometry(NSW_NTT_DET %>% filter(is.na(detoutcome))),  border = 'blue')




## AIMs data
NSW_AIMS_CHR_counts <- 
  
  st_read(dsn = paste0(cultural_data, 
                       'NSW_NATIVE_TITLE_MER.gpkg'),
          layer = 'AHIMS_CHR_Template_Pro') %>% 
  
  st_transform(., st_crs(8058)) %>% as_tibble() %>% 
  
  ## This creates slivers, which need to be removed...
  rename(CHR_Water_Source = CHR_Water_,
         AIMS_Count = Join_Count_1) %>% 
  dplyr::select(CHR_Water_Source, AIMS_Count) %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') 


## Plot the counts
histogram(NSW_AIMS_CHR_counts$AIMS_Count)




## 4.1 :: Native Title determinations ----


## The following method is adapted from the MER (PC ?)
## The goal is to estimate if native title determinations intersect with
## the water sources...


# Consequence rating based on type of claim/determination/LUA that exists with a Water Source
# Native Title claim or determination or LUA exists across the entire Water source = 5
# Native Title claim or determination or LUA exists in part of the water source    = 3
# Native Title claim or determination or LUA does not exist in the water source    = 1



NSW_NTT_values <- NSW_NTT_DET %>% 
  
  # Consequence rating based on type of claim/determination/LUA that exists with a Water Source
  # Native Title claim or determination or LUA exists across the entire Water source = 5
  # Native Title claim or determination or LUA exists in part of the water source = 3
  # Native Title claim or determination or LUA does not exist in the water source  = 1
  mutate(detoutcome = ifelse(is.na(detoutcome), "Native title exists in the entire determination area",   detoutcome),
         DC_4.1_NTT = ifelse(detoutcome ==      "Native title does not exist", 1, 0),
         DC_4.1_NTT = ifelse(detoutcome ==      "Native title exists in parts of the determination area", 3, DC_4.1_NTT),
         DC_4.1_NTT = ifelse(detoutcome ==      "Native title exists in the entire determination area",   5, DC_4.1_NTT)) %>% 
  
  dplyr::select(detoutcome, DC_4.1_NTT) 


plot(st_geometry(NSW_NTT_values %>% filter(detoutcome == "Native title exists in the entire determination area"), border = 'blue'))
plot(st_geometry(NSW_NTT_values %>% filter(detoutcome == "Native title exists in parts of the determination area"), border = 'red'), add = TRUE)
plot(st_geometry(NSW_NTT_values %>% filter(detoutcome == "Native title does not exist"), border = 'green'))



##  INTERSECTIONS ---- 


## Intersect the water sources with the wetlands
message('Intersecting the Native Title and DPEW Geogpraphies')

## This definition is taken from the MER 
## "Consequence rating based on type of claim/determination that exists with a Water Source 
## Native Title claim or determination exists across the entire Water source = 5
## Native Title claim or determination exists in part of the water source = 3
## Native Title claim or determination does not exist in the water source = 1
message('intersecting the Native tile and CHR layers')


NSW_CHR_NTT_int <- 
  
  ## Some water sources intersect multiple NT_LUA areas
  st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated),   
                  st_make_valid(NSW_NTT_values)) %>% 
  
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Under the score column, apply the criteria based on the results of 
  ## the ‘% of Water Source’
  mutate(m2          = st_area(.),
         NTT_Ha      = units::set_units(m2, value = ha),
         NTT_Ha      = drop_units(NTT_Ha),
         
         NTT_Sqkm    = units::set_units(m2, value = km2),
         NTT_Sqkm    = drop_units(NTT_Sqkm),
         NTT_percent = (NTT_Ha/CHR_Ha) * 100 %>% round(.)) %>%
  
  dplyr::select(CHR_Water_Source,  
                detoutcome, 
                DC_4.1_NTT, 
                NTT_Ha, 
                NTT_Sqkm,
                NTT_percent)


NSW_CHR_NTT_int_df <- NSW_CHR_NTT_int %>% as_tibble() %>% select(-geom)


## There are multiples here, not sure why...
NSW_CHR_WS_DC_4.1_NTT <- NSW_CHR_Water_Sources_aggregated %>% 
  
  as_tibble() %>%
  left_join(., NSW_CHR_NTT_int_df, by = "CHR_Water_Source") %>% 
  
  dplyr::select(CHR_Water_Source,
                CHR_Ha,
                CHR_Sqkm,
                detoutcome,
                NTT_Ha,
                NTT_Sqkm,
                NTT_percent,
                DC_4.1_NTT) %>% 
  
  ## For some reason, the intersect is not clean...
  mutate(detoutcome  = ifelse(is.na(detoutcome), 'Native title does not exist', detoutcome), 
         DC_4.1_NTT  = ifelse(detoutcome == 'Native title does not exist', 1,   DC_4.1_NTT), 
         NTT_Ha      = ifelse(detoutcome == 'Native title does not exist', 0,   NTT_Ha), 
         NTT_Sqkm    = ifelse(detoutcome == 'Native title does not exist', 0,   NTT_Sqkm), 
         NTT_percent = ifelse(detoutcome == 'Native title does not exist', 0,   NTT_percent))


## Update
NSW_CHR_WS_DC_4.1_NTT_distinct <- NSW_CHR_WS_DC_4.1_NTT %>% 
  
  ## Some water sources intersect multiple NT_LUA areas
  ## so we have to take the maximum in that case.
  ## 5 over 3, and 3 over 1.
  group_by(CHR_Water_Source) %>% 
  
  ## This leaves no 3s, only 5s
  summarise(NTT_Sqkm    = median(NTT_Sqkm),
            NTT_percent = median(NTT_percent),
            DC_4.1_NTT  = max(DC_4.1_NTT)) 






## 4.2 :: Significant Sites  ----
NSW_CHR_WS_DC_4.2_AIMS <- NSW_CHR_Water_Sources_aggregated %>% 
  
  ## Get the % of each Water source covered by any of the wetlands 
  ## Non-overlapping water sources are 0
  as_tibble() %>% select(-geom) %>% 
  left_join(., NSW_AIMS_CHR_counts, by = "CHR_Water_Source") %>% 
  
  
  mutate(AIMS_density = AIMS_Count/CHR_Sqkm,
         DC_4.2_AIMS  = ntile(AIMS_density, 5)) %>% 
  
  dplyr::select(CHR_Water_Source,  
                AIMS_density,
                DC_4.2_AIMS) 



## Save the tables ----
## Save the table out
write_csv(NSW_CHR_NTT_int_df, 
          paste0(DC4_out, 'NSW_CHR_NTT_int_df.csv'))


st_write(NSW_CHR_NTT_int,
         dsn        = NSW_CHR_WS_Intersections_database_loc,
         layer      = 'NSW_CHR_NTT_int',
         quiet      = TRUE,
         append     = TRUE,
         delete_dsn = TRUE)


write_csv(NSW_CHR_WS_DC_4.1_NTT_distinct, 
          paste0(DC4_out, 'NSW_CHR_WS_DC_4.1_NTT.csv'))


write_csv(NSW_CHR_WS_DC_4.2_AIMS, 
          paste0(DC4_out, 'NSW_CHR_WS_DC_4.2_AIMS.csv'))


## Just Save out the csv files, then join on to the feature layer.





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################