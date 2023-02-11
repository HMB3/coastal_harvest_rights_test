######################################################################################
###############################  ------ HARVESTABLE RIGHTS ---- ######################
######################################################################################



## ENVIRONMENT SETTINGS =============================================================


# \ 
# 
# This code prepares all the data needed to analyse harvest-able rights
#   
#   
#   \

## To-do
threat_species <- FALSE


## Check Ellery's grouping is ok.
## Check the HEVAE and Species data from Jon helps (numbering system).
## Convert the combined data into a proper data bases (HEAVAE and Species)
## Send the combined data to John and Matt for checking
## Check in with Skye on these issues. Ideally, all the data should come correctly formatted from DPEW


## 1a  DC master tables ----
CHR_Decision_Criteria_master_table <- 
  
  read_excel_allsheets(paste0(data_LUT,
                              'Catchment_Based_Decision_Criteria_master_table.xlsx'))


## Themes, criteria and measures
CHR_DC_master_table <- CHR_Decision_Criteria_master_table$DC_LUT %>% 
  mutate_if(is.character, as.factor)


NSW_CHR_WS_FULL_LUT <- read_csv(paste0(catchment_data, 
                                       'NSW_unreg_coastal_water_sources_aggregated_df.csv')) %>% 
  
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')

NSW_CHR_WS_EMU_LUT  <- NSW_CHR_WS_FULL_LUT %>% 
  
  dplyr::select(., CHR_Water_Source, EXTRACTION_MANAGEMENT_UNIT) %>% distinct()



NSW_CHR_WS_EST_LUT  <- select(NSW_CHR_WS_FULL_LUT, 
                              CHR_Water_Source, 
                              Est_No1) %>% distinct() %>% 
  
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')





# 1b SAYERS WS NUMERIC KEY ----


## Read HEVAE excel data - speed this up
HEVAE_ratings <- read_excel_allsheets(paste0(HEVAE_data,
                                             'Coastal_WSP_RiskDatabase_SPOT_08122021_edit.xlsx'))

names(HEVAE_ratings) %>% sort()




## 
SAYERS_WS_LUT <- read_excel_allsheets(paste0(HEVAE_data,
                                             'Water_Source_Numbering_Jon_Sayers.xlsx'))


SAYERS_WS_NUM_LUT <- SAYERS_WS_LUT$`entire Coast Water Sources` %>% 
  
  # rename(CHR_Water_Source = Field_for_join_with_CHR) %>% 
  mutate(Field_for_join_with_CHR = str_trim(Field_for_join_with_CHR),
         Field_for_join_with_CHR = stringr::str_replace_all(Field_for_join_with_CHR, "\\s", " ")) %>% 
  
  select(Field_for_join_with_CHR, 
         IndicatNUM) %>% distinct() %>% na.omit()



## Check "Management Zone" here ---
NSW_CHR_WS_LUT <- 
  
  ## Update
  st_read(dsn = paste0(data_dir, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_CHR_Water_Sources_aggregated')                %>%
  
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') %>%
  as_tibble()          %>% 
  dplyr::select(-geom) %>% 
  
  ## Why are there Management Zones in here? Is that an error?
  # mutate(CHR_Water_Source = gsub("Management Zone", "Water Source", CHR_Water_Source)) %>% 
  mutate(CHR_Water_Source = str_trim(CHR_Water_Source),
         CHR_Water_Source = stringr::str_replace_all(CHR_Water_Source, "\\s", " ")) 


## Regex join of John's numbers to the CHR_WS table ----
NSW_CHR_WS_SAYER_NUM_LUT <- 
  
  
  ## This join needs to be 
  stringdist_full_join(NSW_CHR_WS_LUT, 
                       SAYERS_WS_NUM_LUT, 
                       by = c("CHR_Water_Source" = "Field_for_join_with_CHR"),
                       max_dist = 1) %>% 
  
  rename(Sayers_Water_Source = Field_for_join_with_CHR) %>% 
  
  select(CHR_Water_Source, 
         IndicatNUM)  %>% 
  arrange(IndicatNUM) %>% na.omit() %>% 
  
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')


## 
NSW_CHR_WS_NUM_EMU_LUT <- NSW_CHR_WS_EMU_LUT    %>% 
  
  stringdist_full_join(., NSW_CHR_WS_SAYER_NUM_LUT, 
                       by = "CHR_Water_Source", max_dist = 1) %>% 
  rename(CHR_Water_Source = CHR_Water_Source.x) %>% 
  
  select(., CHR_Water_Source, IndicatNUM, EXTRACTION_MANAGEMENT_UNIT)


NSW_CHR_WS_NUM_LUT <- NSW_CHR_WS_NUM_EMU_LUT %>% 
  select(., CHR_Water_Source, IndicatNUM)    %>% 
  distinct() 



## 
NSW_CHR_WS_NUM_EST_LUT <- NSW_CHR_WS_SAYER_NUM_LUT %>% 
  left_join(., NSW_CHR_WS_EST_LUT,  by = "CHR_Water_Source")



## 482 WS's match
NSW_CHR_WS_NUMKEY_LUT <- select(NSW_CHR_WS_LUT, CHR_Water_Source)  %>% 
  
  ## 
  left_join(., NSW_CHR_WS_NUM_LUT, by = "CHR_Water_Source") %>% 
  distinct(CHR_Water_Source, .keep_all = TRUE) %>% 
  filter(CHR_Water_Source %in% NSW_CHR_WS_LUT$CHR_Water_Source) %>% na.omit()





# 1c HEVAE tables ----


## These dfs are all the same, just bind them together
Towamba_HEVAE_rating <- HEVAE_ratings$`Towamba dd_mm_yyyy` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


BEGA_HEVAE_rating <- HEVAE_ratings$`Bega Area 22_12_2021` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


TWEED_HEVAE_rating <- HEVAE_ratings$`Tweed 21_12_2021` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


RICH_HEVAE_rating <- HEVAE_ratings$`Richmond 07_02_2022` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


BELLIN_HEVAE_rating <- HEVAE_ratings$`Bellinger dd_mm_yyyy` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


COFFS_HEVAE_rating <- HEVAE_ratings$`Coffs Coast 03_06_2020` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


NORTH_HEVAE_rating <- HEVAE_ratings$`Low North Coast 18_06_2020` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


HUNT_HEVAE_rating <- HEVAE_ratings$`Hunter Unreg dd_mm_yyyy` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Water Source'))


METRO1_HEVAE_rating <- HEVAE_ratings$`Metro1 dd_mm_yyyy` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Management Zone'))



METRO2_HEVAE_rating <- HEVAE_ratings$`Metro2 15_12_2021` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>%
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Management Zone'))


METRO3_HEVAE_rating <- HEVAE_ratings$`Metro3 17_12_2021` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Management Zone'))


METRO4_HEVAE_rating <- HEVAE_ratings$`Metro4 20_12_2021` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Management Zone'))


METRO5_HEVAE_rating <- HEVAE_ratings$`Metro5 dd_mm_yyyy` %>%
  .[,colSums(is.na(.))<nrow(.)] %>%
  
  dplyr::select("WaterSourc",
                "RiskType",
                "Consequenc",
                "Likelihood",
                "Risk") %>% 
  
  ## 
  filter(RiskType   == 'freshes' |
           RiskType == '1.5ARI' |
           RiskType == '2.5ARI' | 
           RiskType == 'zero flow periods' |
           RiskType == 'low flows') %>% 
  
  rename(CHR_Water_Source = WaterSourc) %>%
  mutate(CHR_Water_Source = paste0(CHR_Water_Source, ' Management Zone'))


## Columns?
intersect(names(Towamba_HEVAE_rating),
          names(METRO5_HEVAE_rating))


## Combined HEVAE ----
HEAVAE_consequence_all <- bind_rows(Towamba_HEVAE_rating,
                                    BEGA_HEVAE_rating,
                                    TWEED_HEVAE_rating,
                                    RICH_HEVAE_rating,
                                    BELLIN_HEVAE_rating,
                                    COFFS_HEVAE_rating,
                                    NORTH_HEVAE_rating,
                                    HUNT_HEVAE_rating,
                                    METRO1_HEVAE_rating,
                                    METRO2_HEVAE_rating,
                                    METRO3_HEVAE_rating,
                                    METRO4_HEVAE_rating,
                                    METRO5_HEVAE_rating) %>%
  
  ## do we want to remove NAs - could be risky
  na.omit() %>% distinct()                         %>%
  replace_with_na_all(., condition = ~.x == "NA")  %>%
  replace_with_na_all(., condition = ~.x == "N/A") %>%
  
  ## Change categorical ratings to numeric
  mutate(## make the NA's 1 - 
    Consequence_num = case_when((Consequenc == "VH") ~ 5.0,
                                (Consequenc == 'H')  ~ 4.0,
                                (Consequenc == "M")  ~ 3.0,
                                (Consequenc == "L")  ~ 2.0,
                                is.na(Consequenc)    ~ 1.0)) %>% 
  
  ## Remove white space from the
  mutate(CHR_Water_Source = str_trim(CHR_Water_Source))

## May need to join on John's number to 


## Only 250 HEAVE 
length(unique(HEAVAE_consequence_all$CHR_Water_Source))


## Now we check on the HEVAE match
intersect(NSW_CHR_WS_LUT$CHR_Water_Source,
          SAYERS_WS_NUM_LUT$Field_for_join_with_CHR)


## All the differences are the management zones, and the 
setdiff(NSW_CHR_WS_LUT$CHR_Water_Source,
        SAYERS_WS_NUM_LUT$Field_for_join_with_CHR)


## None of the Management zones have HEVAE ratings 
HEAVAE_consequence_combined_max <- HEAVAE_consequence_all %>% 
  
  ## This table needs to be wide, not long
  pivot_wider(.,
              
              names_from  = (c("RiskType")),
              values_from = (c("Consequence_num"))) %>%
  dplyr::select(-Consequenc, -Likelihood, -Risk) %>% 
  
  ## Group by water source and take the max for each category
  group_by(CHR_Water_Source) %>% replace(is.na(.), 1) %>% 
  summarise_if(is.numeric, max, na.rm = TRUE)



## Join John's numbering system
HEAVAE_CONSQ_COMB_NUM <- HEAVAE_consequence_combined_max %>% 
  
  stringdist_full_join(., dplyr::select(NSW_CHR_WS_NUMKEY_LUT, 
                                        CHR_Water_Source, 
                                        IndicatNUM), 
                       by       = "CHR_Water_Source",
                       max_dist = 1) %>% 
  
  rename(CHR_Water_Source_HEVAE = CHR_Water_Source.x,
         CHR_Water_Source       = CHR_Water_Source.y) %>% 
  
  select(CHR_Water_Source_HEVAE, CHR_Water_Source, IndicatNUM, everything())



## 



## Check data ----
## Join on the HEVAE ratings
## For each join, store the difference and the intersect.
## Make a list of tables, col 1 is comparison, col 2 is the result
NSW_CHR_WS_LUT_vs_AGG_HEVAE_ratings_diff <- 
  
  setdiff(unique(NSW_CHR_WS_LUT$CHR_Water_Source),
          unique(HEAVAE_CONSQ_COMB_NUM$CHR_Water_Source))


WS_vs_agg_HEVAE_risk_diff_df <- 
  
  tibble(CHR_Water_Source = NSW_CHR_WS_LUT_vs_AGG_HEVAE_ratings_diff,
         WS_feat_vs_HEAVAE = 'Mismatch')


## Is that true?
NSW_CHR_WS_LUT_vs_AGG_HEVAE_ratings_intersect <- 
  
  intersect(unique(NSW_CHR_WS_LUT$CHR_Water_Source),
            HEAVAE_CONSQ_COMB_NUM$CHR_Water_Source)


WS_vs_agg_HEVAE_risk_match_df <- 
  
  tibble(CHR_Water_Source = NSW_CHR_WS_LUT_vs_AGG_HEVAE_ratings_intersect,
         WS_feat_vs_HEAVAE = 'Match')


WS_vs_HEVAE_risk_all_matches_df <- bind_rows(WS_vs_agg_HEVAE_risk_diff_df,
                                             WS_vs_agg_HEVAE_risk_match_df) %>% 
  
  left_join(., NSW_CHR_WS_NUMKEY_LUT, 
            by = "CHR_Water_Source")


table(WS_vs_HEVAE_risk_all_matches_df$WS_feat_vs_HEAVAE)


## Save tables
write_excel_csv(HEAVAE_consequence_all,
                paste0(HEVAE_data, 'HEAVAE_consequence_values.csv'))

write_csv(HEAVAE_CONSQ_COMB_NUM,
          paste0(HEVAE_data,
                 'CHR_WS_HEAVAE_consequence_combo.csv'))

write_excel_csv(WS_vs_HEVAE_risk_all_matches_df,
                paste0(HEVAE_data, 'WS_vs_agg_HEVAE_risk_all_matches_df.csv'))





# 1c SPATIAL TEMPLATES ----


NSW_CHR_Water_Sources_aggregated <- 
  
  st_read(DPEW_Water_Source_database_loc,
          layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') 


## Use BEGA/BROGO, and TWEED
length(unique(NSW_CHR_WS_FULL_LUT$WATER_SHARING_PLAN))         ## 65 EMUs
length(unique(NSW_CHR_WS_FULL_LUT$MAJOR_CATCH))                ## 34 Catch,ments
length(unique(NSW_CHR_WS_FULL_LUT$EXTRACTION_MANAGEMENT_UNIT)) ## 65 EMUs
length(unique(NSW_CHR_WS_FULL_LUT$CHR_Water_Source))           ## 65 EMUs


EMU_list   <- unique(NSW_CHR_WS_FULL_LUT$EXTRACTION_MANAGEMENT_UNIT) %>% sort()
EMU_test   <- c(EMU_list[5:6], EMU_list[57])
WS_test    <- NSW_CHR_WS_FULL_LUT %>% 
  
  filter(EXTRACTION_MANAGEMENT_UNIT %in% EMU_test) %>% 
  .$CHR_Water_Source %>% unique()   %>% sort()

length(unique(WS_test))           ## 64 Test Water Sources



## Save data ----
## Write the csv table out in the correct format
write_csv(HEAVAE_consequence_combined_max,
          paste0(HEVAE_data,
                 'HEAVAE_consequence_combined_max.csv'))


## 
LUT_table_list <- c('NSW_CHR_WS_FULL_LUT',
                    'SAYERS_WS_NUM_LUT',
                    'NSW_CHR_WS_NUM_EMU_LUT',
                    'NSW_CHR_WS_NUM_LUT',
                    'NSW_CHR_WS_NUM_EST_LUT',
                    'NSW_CHR_WS_NUMKEY_LUT',
                    'HEAVAE_consequence_all',
                    'HEAVAE_CONSQ_COMB_NUM',
                    'WS_vs_HEVAE_risk_all_matches_df')


CHR_WS_LUT_Data_workbook <- createWorkbook()


for(file in LUT_table_list) {
  
  ## Get required columns: file <- tables_list[1]
  File_to_Write <- get(file) %>% as.data.frame()
  
  write_excel_csv(File_to_Write,
                  paste0(economic_data, file, '.csv'))
  
  ## Add worksheet to the spread sheet
  message('writing ', file,  ' to results csv and spreadsheet')
  addWorksheet(CHR_WS_LUT_Data_workbook, file)
  
  ## Write the data to the corresponding worksheet
  writeDataTable(wb         = CHR_WS_LUT_Data_workbook,
                 sheet      = file,
                 x          = get(file),
                 startCol   = 1,
                 startRow   = 1,
                 rowNames   = FALSE,
                 tableStyle = "TableStyleMedium2")
  
}


## Save the whole workbook
saveWorkbook(CHR_WS_LUT_Data_workbook,
             paste0(catchment_data, 'CHR_WS_NUM_HEVAE_LUT_Data.xlsx'),
             overwrite = TRUE)







################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################