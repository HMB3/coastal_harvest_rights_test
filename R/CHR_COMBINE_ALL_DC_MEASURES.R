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



## READ DATA =============================================================



NSW_CHR_Water_Sources_aggregated <- 
  
  st_read(dsn = paste0(data_dir, 
                       'CHR_DPEW_Input_Water_Source_Data.gpkg'),
          layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone') 



## Read in NSW boundaries
NSW_ABS_coast_boundaries <- 
  
  st_read(dsn = paste0(economic_data, 
                       '/ABS/GEOGRAPHIES/G54B_NSW_GDA2020.gpkg'),
          layer = 'G54B_STE_2021_NSW') %>% st_transform(., st_crs(8058)) %>% 
  
  .[NSW_CHR_Water_Sources_aggregated, ] %>% 
  st_crop(., NSW_CHR_Water_Sources_aggregated)



## Environmental profiles ----
NSW_CHR_Water_Sources_enviro_profile <-
  
  read_csv(paste0(enviro_out, 
                  'NSW_CHR_Water_Sources_enviro_profile.csv'))


NSW_CHR_EMU_enviro_profile <- read_csv(
  paste0(enviro_out, 'NSW_CHR_EMU_enviro_profile.csv'))


NSW_CHR_EMU_area <- read_csv(
  paste0(enviro_out, 'NSW_CHR_EMU_enviro_profile.csv'))


# NSW_water_sources_stream_density <- read_csv(paste0(catchment_data, 
#                                                     'NSW_water_sources_stream_density.csv'))
# 
# 
# NSW_EMU_stream_density <- read_csv(paste0(catchment_data, 
#                                           'NNSW_EMU_stream_density.csv'))









## CHR WS Master LUT
NSW_CHR_WS_EST_LUT <- 
  read_csv(paste0(data_dir, 
                  'NSW_CHR_Water_Sources_LUT.csv')) %>% 
  select(CHR_Water_Source, Est_No1) %>% distinct() %>% 
  filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')





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


# 
NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf <- NSW_CHR_Water_Sources_aggregated %>% 
  
  left_join(., Consequence_combined_table, by = "CHR_Water_Source")


NSW_CHR_WS_DC_CONSEQUENCE_SCORES_df <- NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf %>% 
  
  as_tibble() %>% select(-geom)



## PLOT MEASURES ----


## Create lists of measures
DC_plots <- c("DC_1.1_HEVAE",	
              "DC_1.2_WET",	
              "DC_1.3_MACRO",	
              "DC_1.4_CARBON",	
              "DC_1.7_CAPAD",	
              "DC_2.2_AV_EXTR_LOW",	
              "DC_3.1_EST_HEALTH",	
              "DC_3.2_ALGAL_low", 
              "DC_4.1_NTT",	
              "DC_4.2_AIMS",	
              "DC_5.1_SEIFA",	
              "DC_5.2_UNEMP",	
              "DC_5.5_FISH",	
              "DC_5.6_BOAT",	
              "DC_6.1_TOUR", 
              "DC_6.2_AQUACULT",	
              "DC_7.1_WATER_Ha", 
              "DC_7.2_WATER_Pop",	
              "DC_8.1_WATER_USE",	
              "DC_8.2_GROSS",	
              "DC_8.3_AG_EMP")


## Create a list of plots for the report
DC_CONSEQ_plot_list <- DC_plots %>%
  
  ## Pipe the list into lapply
  ## DC <-  DC_plots[1]
  lapply(function(DC) {
    
    message('Plot maps for ', DC)
    
    tm_shape(NSW_ABS_coast_boundaries) +
      tm_polygons(col = 'grey') +
      tm_shape(NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf) +
      
      tm_fill(col     = DC,
              palette = "YlOrRd",
              style   = "fixed",
              n       = 5,
              breaks  = c(1, 2, 3, 4, 5, +Inf), 
              labels  = c("1", "2", "3", "4", "5"),
              textNA  = "No Data") +
      
      tm_borders() +
      
      tm_layout(legend.outside       = TRUE,
                legend.width         = 1.3,
                panel.label.height   = 1.3,
                panel.label.size     = 1.3,
                legend.title.size    = 3, 
                legend.text.size     = 1.2,
                
                frame                = FALSE,
                # main.title           = DC, 
                main.title.position  = 'centre',
                main.title.size      = 1.8,
                main.title.fontface  = "bold.italic") 
  }) %>% c()


## Rename the list items
names(DC_CONSEQ_plot_list) <- DC_plots
DC_CONSEQ_plot_list[["DC_1.1_HEVAE"]]
DC_CONSEQ_plot_list[["DC_5.5_FISH"]]
DC_CONSEQ_plot_list[["DC_5.5_FISH"]]





## Save the plots for reporting
DC_plots %>%
  
  ## Pipe the list into lapply
  ## DC <-  DC_plots[1]
  lapply(function(DC) {
    
    DC_map <- tm_shape(NSW_ABS_coast_boundaries) +
      tm_polygons(col = 'grey') +
      tm_shape(NSW_CHR_WS_DC_CONSEQUENCE_SCORES_sf) +
      
      tm_fill(col     = DC,
              palette = "YlOrRd",
              style   = "fixed",
              n       = 5,
              breaks  = c(1, 2, 3, 4, 5, +Inf), 
              labels  = c("1", "2", "3", "4", "5"),
              textNA  = "No Data") +
      
      tm_borders() +
      
      tm_layout(legend.outside       = TRUE,
                legend.width         = 1.3,
                panel.label.height   = 1.3,
                panel.label.size     = 1.3,
                legend.title.size    = 3, 
                legend.text.size     = 1.2,
                
                frame                = FALSE,
                # main.title           = DC, 
                main.title.position  = 'centre',
                main.title.size      = 1.8,
                main.title.fontface  = "bold.italic")
    
    message('Plot maps for ', DC)
    png(paste0(conseq_out, DC, '_Consequence_Map.png'),
        10, 6, units = 'in', res = 600)
    
    print(DC_map)
    
    dev.off()
    
  })



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
#########################################################################################################################