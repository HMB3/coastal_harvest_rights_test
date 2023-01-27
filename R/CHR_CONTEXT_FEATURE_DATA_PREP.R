######################################################################################
###############################  ------ HARVESTABLE RIGHTS ---- ######################
######################################################################################



## ENVIRONMENT SETTINGS =============================================================


# \ 
# 
# This code prepares all the socio-economic data needed to analyse harvest-able rights
#   
#   
#   \

## To-do

## Check ABS




# 3 :: Context Feature data ----



if(read_context) {
  
  
  ## CHR WS
  NSW_CHR_Water_Sources_aggregated <- 
    
    st_read(dsn = paste0(data_dir, 
                         'CHR_DPEW_Input_Water_Source_Data.gpkg'),
            layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
    filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') 
  
  
  ## 3a ABS Data ----
  
  
  ## Read in the Project feature data here from the inputs geo-database
  ABS_CEN2021_SA1_coastal <- 
    
    st_read(dsn = paste0(economic_data, 
                         '/ABS/GEOGRAPHIES/SA1_2021_AUST_GDA2020.shp')) %>% 
    
    st_transform(., st_crs(8058)) %>% 
    dplyr::select(SA1_CODE21, SA2_NAME21, AREASQKM21, geometry) %>% 
    
    .[NSW_CHR_Water_Sources_aggregated, ]
  
  
  ## Read in the Project feature data here from the inputs geo-database
  ABS_CEN2021_LGA_coastal <- 
    
    st_read(dsn = paste0(economic_data, 
                         '/ABS/GEOGRAPHIES/ABS_G01_LGA_AUST_GDA2020.gpkg'),
            layer = 'ABS_G01_LGA_AUST_GDA2020') %>% 
    
    st_transform(., st_crs(8058)) %>% 
    
    ## Calculate the areas
    dplyr::mutate(m2       = st_area(geom),
                  LGA_Ha   = units::set_units(m2, value = ha),
                  LGA_Ha   = drop_units(LGA_Ha),
                  
                  LGA_Sqkm = units::set_units(m2, value = km2),
                  LGA_Sqkm = drop_units(LGA_Sqkm),
                  
                  LGA_NAME_2021 = toupper(LGA_NAME_2021), 
                  LGA_NAME_2021 = sub(" .*", "", LGA_NAME_2021)) %>% 
    
    dplyr::select(id, LGA_CODE_2021,
                  LGA_NAME_2021,
                  LGA_Ha, LGA_Sqkm,
                  everything(),
                  geom) %>% 
    
    .[NSW_CHR_Water_Sources_aggregated, ]
  
  
  
  ABS_CEN2021_POA_coastal <- 
    
    st_read(dsn = paste0(economic_data, 
                         '/ABS/GEOGRAPHIES/ABS_G01_POA_AUST_GDA2020.gpkg'),
            layer = 'ABS_G01_POA_AUST_GDA2020') %>% 
    
    st_transform(., st_crs(8058)) %>% 
    
    ## Calculate the areas
    dplyr::mutate(m2       = st_area(geom),
                  POA_Ha   = units::set_units(m2, value = ha),
                  POA_Ha   = drop_units(POA_Ha),
                  
                  POA_Sqkm = units::set_units(m2, value = km2),
                  POA_Sqkm = drop_units(POA_Sqkm)) %>% 
    
    dplyr::select(id, POA_CODE_2021,
                  POA_NAME_2021,
                  POA_Ha, POA_Sqkm, AREA_ALBERS_SQKM,
                  everything(),
                  geom) %>% 
    
    .[NSW_CHR_Water_Sources_aggregated, ]
  
  
  ## This needs to have all the geography in it?
  ABS_LGA_POA_coastal_LUT_sf <-   
    
    st_intersection(st_make_valid(dplyr::select(ABS_CEN2021_LGA_coastal, 
                                                LGA_CODE_2021, LGA_NAME_2021)), 
                    
                    st_make_valid(dplyr::select(ABS_CEN2021_POA_coastal, 
                                                POA_CODE_2021, POA_NAME_2021))) %>% 
    
    st_intersection(., st_make_valid(NSW_CHR_Water_Sources_aggregated)) 
  
  
  ABS_LGA_POA_coastal_LUT_df <- ABS_LGA_POA_coastal_LUT_sf %>% 
    
    as_tibble() %>% dplyr::select(CHR_Water_Source,
                                  LGA_NAME_2021, 
                                  POA_CODE_2021, 
                                  POA_NAME_2021) %>% 
    distinct()
  
  
  ## Need to get the SA1 - POA - LGA - WS Hierarchy set up 
  
  
  ## SEIFA Indexes
  SEIFA_LGA_Indexes <- read_excel(paste0(economic_data,
                                         'ABS/LGA_IRSAD.xlsx'),
                                  sheet = 'IRSAD')
  
  
  SEIFA_SA1_Indexes <- read_excel(paste0(economic_data,
                                         'ABS/SA1_IRSAD.xlsx'),
                                  sheet = 'IRSAD') %>% 
    filter(State == 'NSW') %>% 
    
    dplyr::select(SA1_11Digit_Code, 
                  Population, 
                  IRSADS_Score, 
                  NSW_Rank, 
                  NSW_Decile,
                  NSW_Percentile)
  
  
  ## Employment
  SA1_Agriculture <- read_excel(paste0(economic_data,
                                       'ABS/SA1_Agriculture.xlsx'),
                                sheet = 'SA1_Agriculture') 
  
  
  SA1_Agriculture_total <- SA1_Agriculture %>% 
    
    rename(Agg_Total        = Total,
           SA1_11Digit_Code = SA1_CODE) %>% 
    
    dplyr::select(SA1_11Digit_Code, Agg_Total)
  
  
  ## Unemployed 
  SA1_Unemployed <- read_excel(paste0(economic_data,
                                      'ABS/SA1_Emplpoyment.xlsx'),
                               sheet = 'SA1_Emplpoyment') %>% 
    
    rename(Employment_Total = Total)
  
  
  ## Create one table at SA1 of SEFIA IRSAD, % unemployment & % work in Agriculture
  SA1_combo <- SEIFA_SA1_Indexes %>% left_join(., SA1_Agriculture_total,
                                               by = "SA1_11Digit_Code") %>% 
    
    ## For more joins, make this a list
    left_join(., SA1_Unemployed,
              by = "SA1_11Digit_Code") %>% replace(is.na(.), 0) %>% 
    
    ## Make the NAs zero
    ## Calculate working % from employed + unemployed.
    mutate(Agriculture_percent = (Agg_Total/Employment_Total)   * 100 %>% round(., 1),
           Unemployed_percent  = (Unemployed/Employment_Total)  * 100 %>% round(., 1),
           Unemployed_percent  = ifelse(Unemployed_percent > 100, 100, Unemployed_percent)) %>% 
    
    as_tibble() %>% 
    mutate(SA1_11Digit_Code = as.character(SA1_11Digit_Code))
  
  SA1_combo[nan_df(SA1_combo)] <- 0
  
  
  ## Population by SA1 - just for a cross-check
  ## This is from AURIN. Area Field looks dodgy.
  AURIN_SA1_NSW_Population <- 
    read_csv(paste0(economic_data,
                    '/ABS/AURIN_SA1_aggrega_pop_and_dwelling_counts_census_2016.csv')) %>% 
    filter(state_name == "New South Wales") %>% 
    rename(SA1_11Digit_Code = sa1_maincode_2016) %>% 
    dplyr::select(SA1_11Digit_Code, population, dwelling, area_sqkm) %>% 
    replace(is.na(.), 0)
  
  
  ## 3b Maritime data ----
  
  
  ## BOAT licences by POA - save to csv =
  POA_Boat_licences <- read_excel(paste0(economic_data,
                                         'Maritime/NSW_Boat _Rego_POA _Oct_2022.xlsx'),
                                  sheet = 'Boat_licences') %>% 
    
    mutate(POA_length = nchar(POA))  %>% 
    filter(POA_length == 4)          %>% 
    replace(is.na(.), 0)             %>% 
    mutate(POA = paste0('POA', POA)) %>% 
    rename(POA_CODE_2021 = POA)
  
  
  ## Get the intersect between the two lists
  match_poa <- intersect(unique(ABS_CEN2021_POA_coastal$POA_CODE_2021), 
                         unique(POA_Boat_licences$POA_CODE_2021))
  
  diff_poa <- setdiff(unique(ABS_CEN2021_POA_coastal$POA_CODE_2021), 
                      unique(POA_Boat_licences$POA_CODE_2021))
  
  
  ## Fishing licences
  ## BOAT licences by POA - save to csv =
  POA_Fish_licences <- read_excel(paste0(economic_data,
                                         'Maritime/Rec_Fish_Lic_Sold_POA_5_yrs.xlsx'),
                                  sheet = 'Sheet1') %>% 
    
    rename(Fish_total = "COUNT(DISTINCT Licence Number)",
           POA_CODE_2021 = "Post Code") %>% 
    mutate(POA_CODE_2021 = paste0('POA', POA_CODE_2021)) %>% 
    group_by(POA_CODE_2021) %>% 
    summarise(Fish_total = sum(Fish_total)) 
  
  
  ## Join POA_layer to data
  ABS_CEN2021_POA_coastal_Boat_fish <- ABS_CEN2021_POA_coastal %>% 
    
    left_join(., select(POA_Boat_licences, POA_CODE_2021, Boat_Total),
              by = "POA_CODE_2021") %>% 
    
    left_join(., POA_Fish_licences,
              by = "POA_CODE_2021") %>% 
    
    select(POA_CODE_2021, 
           POA_NAME_2021, 
           POA_Ha,
           POA_Sqkm, 
           Boat_Total,
           Fish_total) %>% 
    
    ## Some Postcodes have no fishing or boating licences
    replace(is.na(.), 0)
  
  
  ## Link the ABS data to the water source
  ## This needs to have all the geography in it?
  WS_ABS_SA1_Intersect_sf <-   
    
    st_intersection(
      
      st_make_valid(NSW_CHR_Water_Sources_aggregated), 
      
      st_make_valid(ABS_CEN2021_SA1_coastal)) %>% 
    
    dplyr::select(CHR_Water_Source,
                  SA1_CODE21, SA2_NAME21) 
  
  
  WS_ABS_SA1_Intersect_df <- WS_ABS_SA1_Intersect_sf %>% 
    
    as_tibble() %>% 
    dplyr::select(CHR_Water_Source, SA1_CODE21, SA2_NAME21) %>% 
    
    rename(SA1_11Digit_Code = SA1_CODE21) %>% 
    mutate(SA1_11Digit_Code = as.numeric(SA1_11Digit_Code))
  
  
  ## 
  WS_ABS_SA1_combo_index <- WS_ABS_SA1_Intersect_df %>% 
    
    mutate(SA1_11Digit_Code = as.character(SA1_11Digit_Code)) %>% 
    left_join(., SA1_combo, by = "SA1_11Digit_Code")
  
  
  WS_ABS_combo_index  <- WS_ABS_SA1_combo_index %>% 
    
    group_by(CHR_Water_Source) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    replace(is.na(.), 0)
  
  
  ## What are the dimensions here?
  length(unique(ABS_CEN2021_POA_coastal$POA_CODE_2021)) ## 468 postcodes
  length(unique(ABS_CEN2021_LGA_coastal$LGA_NAME_2021)) ## 85 LGAs.
  length(unique(ABS_CEN2021_SA1_coastal$SA1_CODE21))    ## 17396
  
  
  ## Join Indices
  NSW_CHR_Water_Sources_aggregated_ABS <- 
    
    NSW_CHR_Water_Sources_aggregated %>%
    left_join(., WS_ABS_combo_index,
              by = c("CHR_Water_Source"))
  
  
  ## 3c Economic data ----
  
  
  ## Water Use ----
  NSW_LGA_Water_utilities <- read_csv(paste0(economic_data,
                                             'NSW_LGA_Water_utilities.csv')) %>%
    
    dplyr::select(LGA, Population_2014, Annual_Water_Demand_2014_ML) %>% na.omit() %>% 
    rename(#Population_2014        =  "Population  Served 2014",
      Town_Ann_Water_Demand_2014_ML = Annual_Water_Demand_2014_ML,
      LGA_NAME_2021                = LGA) %>% 
    mutate(LGA_NAME_2021 = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA_NAME_2021)))
  
  
  
  ## Update 
  Annual_LGA_AG_water_use <- read_csv(paste0(economic_data,
                                             'Agr_Annual_water_use_per_area.csv')) %>%
    
    dplyr::select(LGA_CODE_2, LGA_NAME_2, "Annual water use") %>% na.omit() %>% 
    rename(LGA_NAME_2021    = LGA_NAME_2,
           AG_Annual_water_use = "Annual water use") %>% 
    mutate(LGA_NAME_2021 = toupper(LGA_NAME_2021)) %>% 
    mutate(LGA_NAME_2021 = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA_NAME_2021)))
  
  
  
  NSW_LGA_Water_Combo <- left_join(NSW_LGA_Water_utilities,
                                   Annual_LGA_AG_water_use, by = 'LGA_NAME_2021')
  
  
  write_csv(NSW_LGA_Water_Combo, paste0(economic_data, 'NSW_LGA_Water_Combo.csv'))
  
  
  ## Gross margins ----
  Gross_margins_full <- read_excel(paste0(economic_data,
                                          'Context_Input_Data _Gross_margins.xlsx'),
                                   sheet = 'NSW_LUE_Secondary_freq (GM)')
  
  
  Gross_margins_values <- Gross_margins_full %>% 
    filter(Low > 0) %>% 
    select(`Secondary LUE`, Low, Mid, high, Farm_Dams, Opp_cost) %>% 
    rename(Secondary_LUE = `Secondary LUE`)
  
  
  ## Tourism businesses
  Tourism_businesses <- read_excel(paste0(economic_data,
                                          'NSW_Tourism_Businesses_LGA_Scaled_1_5.xlsx'),
                                   sheet = 'Tourism') %>% 
    
    select(LGA, NSW_LGA_3, UI, "TN of Tourism Businesses", "Scaled TN of Tourism Businesses") %>%
    rename(Tour_Biz_Tot        = "TN of Tourism Businesses",
           Scaled_Tour_Biz_Tot = "Scaled TN of Tourism Businesses") %>% 
    
    rename(LGA_NAME_2021 = NSW_LGA_3) %>%
    mutate(LGA_NAME_2021 = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA_NAME_2021)))
  
  
  
  
  
  ## 3d Indigenous data ----
  
  
  NSW_NTT_DET <- 
    
    st_read(dsn = paste0(cultural_data, 
                         'SEED_WFS/NSW_NTT_DET.gpkg'),
            layer = 'NSW_NTT_DET') %>% 
    st_transform(., st_crs(8058))  %>%  
    
    st_make_valid()   %>% 
    st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() 
  
  
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
  
  
  
  ## 3e Land use ----
  if(land_use) {
    
    
    ##
    NSW_land_use <- st_read('./data/enviro/Land_use/NSW_land_use_coastal_ALB.shp') %>% 
      st_transform(., st_crs(8058))
    
    NSW_soils    <- st_read('./data/enviro/Soils/SoilType_ASC_NSW_ALB.shp') %>% 
      st_transform(., st_crs(8058))
    
    
    ## Classify the LUE categories into numeric data
    NSW_land_use_Secondary_numerics <- NSW_land_use %>% 
      
      ## Change the land-use values.
      ## These need to be scaled?
      mutate(Secondary_ord = case_when(grepl("1.1.0 Nature conservation",          Secondary) ~ 1.1,
                                       grepl("1.2.0 Managed resource protection",  Secondary) ~ 1.2,
                                       grepl("1.3.0 Other minimal use",            Secondary) ~ 1.3,
                                       grepl("2.1.0 Grazing native vegetation",    Secondary) ~ 2.1,
                                       
                                       ##
                                       grepl("2.2.0 Production native forestry",   Secondary) ~ 2.2,
                                       grepl("3.1.0 Plantation forests",           Secondary) ~ 3.1,
                                       grepl("3.2.0 Grazing modified pastures",    Secondary) ~ 3.2,
                                       grepl("3.3.0 Cropping",                     Secondary) ~ 3.3,
                                       
                                       ##
                                       grepl("3.4.0 Perennial horticulture",       Secondary) ~ 3.4,
                                       grepl("3.5.0 Seasonal horticulture",        Secondary) ~ 3.5,
                                       grepl("3.6.0 Land in transition",                  Secondary) ~ 3.6,
                                       grepl("4.1.0 Irrigated plantation forests",        Secondary) ~ 4.1,
                                       
                                       ## 
                                       grepl("4.2.0 Grazing irrigated modified pastures", Secondary) ~ 4.2,
                                       grepl("4.3.0 Irrigated cropping",                  Secondary) ~ 4.3,
                                       grepl("4.4.0 Irrigated perennial horticulturee",   Secondary) ~ 4.4,
                                       grepl("4.5.0 Irrigated seasonal horticulture",     Secondary) ~ 4.5,
                                       
                                       ##
                                       grepl("4.6.0 Irrigated land in transition",        Secondary) ~ 4.6,
                                       grepl("5.1.0 Intensive horticulture",              Secondary) ~ 5.1,
                                       grepl("5.2.0 Intensive animal production" ,        Secondary) ~ 5.2,
                                       grepl("5.3.0 Manufacturing and industrial",        Secondary) ~ 5.3,
                                       
                                       ##
                                       grepl("5.4.0 Residential and farm infrastructure", Secondary) ~ 5.4,
                                       grepl("5.5.0 Services",                            Secondary) ~ 5.5,
                                       grepl("5.6.0 Utilities" ,                          Secondary) ~ 5.6,
                                       grepl("5.7.0 Transport and communication",         Secondary) ~ 5.7,
                                       
                                       grepl("5.8.0 Mining",                              Secondary) ~ 5.8,
                                       grepl("5.9.0 Waste treatment and disposal",        Secondary) ~ 5.9,
                                       grepl("6.1.0 Lake" ,                               Secondary) ~ 6.1,
                                       grepl("6.2.0 Reservoir/dam",                       Secondary) ~ 6.2,
                                       
                                       grepl("6.3.0 River",                               Secondary) ~ 6.3,
                                       grepl("6.4.0 Channel/aqueduct",                    Secondary) ~ 6.4,
                                       grepl("6.5.0 Marsh/wetland" ,                      Secondary) ~ 6.5,
                                       grepl("6.6.0 Estuary/coastal waters",              Secondary) ~ 6.6)) %>% 
      
      rename(Secondary_LUE = Secondary)
    
    
    ## Aggregate landuse into separate 
    NSW_LUE_Secondary_area <- NSW_land_use_Secondary_numerics %>% 
      
      group_by(Secondary_LUE, Secondary_ord) %>% 
      
      ## This creates slivers, which need to be removed...
      summarize()              %>% 
      st_make_valid()          %>% 
      st_buffer(., 0.0)        %>% 
      nngeo::st_remove_holes() %>% 
      
      dplyr::mutate(m2       = st_area(geometry),
                    LUE_Ha   = units::set_units(m2, value = ha),
                    LUE_Ha   = drop_units(LUE_Ha),
                    
                    LUE_Sqkm = units::set_units(m2, value = km2),
                    LUE_Sqkm = drop_units(LUE_Sqkm)) %>% 
      
      dplyr::select(Secondary_LUE,
                    Secondary_ord,
                    LUE_Ha,
                    LUE_Sqkm)
    
    
    ## Create a LUT for the LUE raster
    NSW_LUE_LUT <- NSW_land_use_Secondary_numerics %>% as_tibble() %>% 
      
      dplyr::select(Secondary_LUE, Tertiary, Secondary_ord)        %>% 
      distinct() 
    
    
    NSW_LUE_Secondary_freq <- NSW_land_use_Secondary_numerics %>% as_tibble() %>% 
      
      dplyr::select(Secondary_LUE) %>% 
      tbl_summary(sort = all_categorical() ~ "frequency") %>% 
      # add_n() %>%
      # as_gt() %>%
      as_tibble() %>% .[-1,]
    
    names(NSW_LUE_Secondary_freq) <- c('Secondary LUE', 'Count')
    
    
    NSW_LUE_Tertiary_freq <- NSW_land_use_Secondary_numerics %>% as_tibble() %>% 
      
      dplyr::select(Tertiary) %>% 
      tbl_summary(sort = all_categorical() ~ "frequency") %>% 
      as_tibble() %>% .[-1,]  
    
    
    ## Save a table of all the LUE features
    NSW_LUE_Secondary_Features_Gross_Margins <- NSW_land_use_Secondary_numerics %>% 
      
      left_join(., Gross_margins_values, by = "Secondary_LUE")
    
    
    ## Save a table of aggregated LUE features
    NSW_2ND_LUE_GM <- NSW_LUE_Secondary_area %>% 
      
      left_join(., Gross_margins_values, by = "Secondary_LUE")
    

    
    ## Soils
    NSW_soils_numeric <- NSW_soils %>% 
      
      ## Classify the LUE categories into numeric data
      
      ## Change the land-use values
      ## These need to relate to % clay, etc. Which we already have CSIRO data for
      mutate(ASC_num = case_when(grepl("Calcarosols",   ASC_order) ~ 1.0,
                                 grepl("Chromosols",    ASC_order) ~ 2.0,
                                 grepl("Dermosols",     ASC_order) ~ 3.0,
                                 grepl("Ferrosols",     ASC_order) ~ 4.0,
                                 
                                 ##
                                 grepl("Hydrosols",     ASC_order) ~ 5.0,
                                 grepl("Kandosols",     ASC_order) ~ 6.0,
                                 grepl("Kurosols",      ASC_order) ~ 7.0,
                                 grepl("Organosols",    ASC_order) ~ 8.0,
                                 grepl("Tenosols",      ASC_order) ~ 9.0,
                                 
                                 ##
                                 grepl("Tenosols",          ASC_order) ~ 10.0,
                                 grepl("Ferrosols",         ASC_order) ~ 11.0,
                                 grepl("(natric)",          ASC_order) ~ 12.0,
                                 grepl("Vertosols",         ASC_order) ~ 13.0,
                                 
                                 ## 
                                 grepl("Rudosols (alluvial)", ASC_order) ~ 14.0,
                                 grepl("Podosols",            ASC_order) ~ 15.0,
                                 grepl("Hydrosols",           ASC_order) ~ 16.0,
                                 grepl("Rudosols",            ASC_order) ~ 17.0,
                                 
                                 ##
                                 grepl("Water",               ASC_order) ~ 18.0,
                                 grepl("Sodosols",            ASC_order) ~ 19.0,
                                 grepl("Not assessed",        ASC_order) ~ 20.0)) 
    
    
    ## Create a LUT for the LUE raster
    NSW_ASC_LUT <- NSW_soils_numeric %>% as_tibble() %>% 
      
      dplyr::select(ASC_order, ASC_code, ASC_num) %>% 
      distinct() %>% 
      arrange(ASC_order)
    
    
    NSW_Soils_freq <- NSW_soils %>% as_tibble() %>% 
      
      dplyr::select(ASC_order) %>% 
      tbl_summary(sort = all_categorical() ~ "frequency") %>% 
      as_tibble() %>% .[-1,]
    
    names(NSW_Soils_freq) <- c('Soil ASC', 'Count')
    
  }
  
  
  ## Aquaculture
  Aquaculture_leases <- st_read(dsn = paste0(economic_data, 
                                             'Aquaculture/Aquaculture_Leases.shp')) %>% 
    st_transform(., st_crs(8058)) 
  
  
  
  
  
  ## Save data ----
  if(write_context) {
    
    
    ## 
    context_table_list <- c('ABS_LGA_POA_coastal_LUT_df',
                            'SA1_combo',
                            'WS_ABS_SA1_Intersect_df',
                            'WS_ABS_SA1_combo_index',
                            'WS_ABS_combo_index',
                            # 'NSW_CHR_WS_LUE_int_df',
                            'AURIN_SA1_NSW_Population',
                            'Gross_margins_full',
                            'Gross_margins_values',
                            'NSW_LGA_Water_Combo',
                            'POA_Boat_licences',
                            'POA_Fish_licences',
                            'Tourism_businesses')
    
    
    ## Context
    context_layer_list <- c('NSW_NTT_DET', 
                            'NSW_AIMS_CHR_counts',
                            'ABS_CEN2021_SA1_coastal',
                            'ABS_CEN2021_LGA_coastal',
                            'ABS_CEN2021_POA_coastal',
                            'ABS_CEN2021_POA_coastal_Boat_fish',
                            'NSW_LUE_Secondary_Features_Gross_Margins',
                            'NSW_2ND_LUE_GM',
                            'ABS_LGA_POA_coastal_LUT_sf',
                            'WS_ABS_SA1_Intersect_sf',
                            'NSW_CHR_Water_Sources_aggregated_ABS')
    
    
    CHR_DPEW_Input_Data_workbook <- createWorkbook()
    
    for(file in context_table_list) {
      
      ## Get required columns: file <- tables_list[1]
      File_to_Write <- get(file) %>% as.data.frame()
      
      write_excel_csv(File_to_Write,
                      paste0(economic_data, file, '.csv'))
      
      ## Add worksheet to the spread sheet
      message('writing ', file, ' to results csv and spreadsheet')
      addWorksheet(CHR_DPEW_Input_Data_workbook, file)
      
      ## Write the data to the corresponding worksheet
      writeDataTable(wb         = CHR_DPEW_Input_Data_workbook,
                     sheet      = file,
                     x          = get(file),
                     startCol   = 1,
                     startRow   = 1,
                     rowNames   = FALSE,
                     tableStyle = "TableStyleMedium2")
      
    }
    
    ## Save the whole workbook
    saveWorkbook(CHR_DPEW_Input_Data_workbook,
                 paste0(economic_data, 'CHR_DPEW_Context_Input_Data.xlsx'),
                 overwrite = TRUE)
    
  }
  
  
  ## Save the spatial data
  if(write_context) {
    
    message('saving DPEW Input data to Geo-package')
    NSW_Context_Layers_database_loc <- paste0(economic_data, 'CHR_NSW_Input_Context_Layers_Data.gpkg')
    NSW_Context_Layers_gdb_loc      <- paste0(economic_data, 'CHR_NSW_Input_Context_Layers_Data.gdb')
    
    if(file.exists(NSW_Context_Layers_database_loc)) {
      message('re-create geo-package')
      file.remove(NSW_Context_Layers_database_loc)
    }
    
    for(layer in context_layer_list) {
      
      ## layer <- context_layer_list[1]
      File_to_Write <- get(layer) 
      
      #message('writing ', layer, ' to geo database')
      # arc.write(path     = DPEW_Water_Source_gdb_loc,
      #           data     = File_to_Write,
      #           validate = TRUE)
      
      message('writing ', layer, ' to geo-package')
      st_write(File_to_Write,
               dsn        = NSW_Context_Layers_database_loc,
               layer      = layer,
               quiet      = TRUE,
               append     = TRUE,
               delete_dsn = TRUE)
      
    }
    
    # Save the tables to an SQL
    context_db = dbConnect(RSQLite::SQLite(),
                           dbname = NSW_Context_Layers_database_loc)
    
    dbListTables(context_db)
    
  }
}







################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################