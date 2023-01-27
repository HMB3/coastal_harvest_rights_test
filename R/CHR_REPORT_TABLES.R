###################################################################################################
###################################  ------ CHR REPORT TABLES ---- #################################
###################################################################################################


# \ 
# 
# This code creates the EMU and WS flex tables for the .pdf reports
#   
#   
#   \


## ENVIRONMENT SETTINGS =============================================================





# STEP 1 :: create EMU Flex tables ----


## Create numbers for the table
EMU_vals <- NSW_CHR_EMU_enviro_profile %>% 
  filter(EXTRACTION_MANAGEMENT_UNIT == EMU_test)

EMU_dens <- NSW_CHR_EMU_stream_density %>% 
  filter(EXTRACTION_MANAGEMENT_UNIT == EMU_test)

emu_number_of_ws <- NSW_CHR_Water_Sources_LUT    %>% 
  filter(EXTRACTION_MANAGEMENT_UNIT == EMU_test) %>% 
  .$CHR_Water_Source %>% unique() %>% length()

emu_area <- EMU_vals$EMU_Sq_km %>% round()

land_use_cat  <- EMU_LUE_Secondary_freq$`Secondary LUE`[1]
land_use_perc <- EMU_LUE_Secondary_freq$Count[1]

emu_dam_area <- EMU_dam_feat %>% st_area(geometry) %>% `/`(sqkm_conversion) %>% drop_units()
dam_perc     <- (emu_dam_area/emu_area) *100 %>% round(., 1)



## EMU tibble ----
EMU_summary_tibble <- tibble(Characteristics =
                               
                               ## Create headline numbers 
                               c("Number of Water Sources",
                                 "Catchment Size (km2)",
                                 "Annual Precipitation (mm)",
                                 "Stream Density (km2)",
                                 "Forest Cover",
                                 "Water Dependent Assets",
                                 "Major Estuaries",
                                 "Major Towns",
                                 "Population",
                                 "Agricltural Land Use",
                                 "Agricltural Output",
                                 "Current HR Uptake (ML)",
                                 "% Area Eligible for HR",
                                 "Adjacent EMUs"),
                             
                             ## Enter headline values
                             Values =
                               
                               c(emu_number_of_ws   %>% round(),
                                 EMU_vals$EMU_Sq_km %>% round(),
                                 EMU_vals$median.Annual_precip %>% round(),
                                 emu_area,
                                 EMU_vals$max.Forest_Cov %>% round(),
                                 "Wetland 1/2/3",
                                 "Estuaries 1/2/3",
                                 "Bega / Merimbula / Perimbula",
                                 87150,
                                 paste0(land_use_cat, ' ', land_use_perc),
                                 '550M',
                                 10,
                                 dam_perc,
                                 "EMUs 1/2/3")) %>% 
  
  ## Turn into flextable
  flextable() %>% 
  
  ## Set background formatting for flext table
  bg(bg = "springgreen3", part = "header") %>% 
  bg(bg = "wheat",        part = "body") %>%
  
  ## Fontsize, color and bold 
  fontsize(size = 14,  part = "header") %>%     
  fontsize(size = 10,  part = "body")  %>% 
  bold(i = 1,  j = NULL, bold = TRUE, part = "header")  %>%
  
  ## Set the colour and width
  # color(color = "white", i=~ Values == "Count") %>%
  # color(color = "white", part = "all")          %>% 
  
  width(j =~ Characteristics,  width = 100) %>%
  width(j =~ Values,           width = 30) %>%
  height_all(., height = 10)





## EMU Flow tibble ----
EMU_flow_tibble <- as_tibble(data.frame(matrix(nrow = 5, ncol = 6)))
names(EMU_flow_tibble) <- c("Variable", 
                            "10%",
                            "20%",
                            "30%",
                            "40%",
                            "50%")


EMU_flow_tibble$Variable <- c("High Flow",
                              "Low flow",
                              "Durations",
                              "Freshes",
                              "Days below cease to pump")

# EMU_flow_tibble$`10%` <- 13

EMU_flow_flex <- EMU_flow_tibble %>%
  
  ## Turn into flextable
  flextable() %>% 
  
  ## Set background formatting for flext table
  bg(bg = "springgreen3", part = "header") %>% 
  bg(bg = "wheat",        part = "body") %>%
  
  ## Fontsize, color and bold 
  fontsize(size = 14,  part = "header") %>%     
  fontsize(size = 10,  part = "body")  %>% 
  bold(i = 1,  j = NULL, bold = TRUE, part = "header")  %>%
  
  ## Set the colour and width
  height_all(., height = 5)




# Summary EMU Risk Flex table ----
CHR_DC_EMU_master_flex <- CHR_DC_EMU_master_table %>% 
  
  ## 
  flextable() %>% 
  
  ## Set background formatting for flext table
  bg(bg = "springgreen3", part = "header") %>% 
  bg(bg = "wheat",        part = "body")   %>%
  
  ## Colour the cells for current Risks ----
  bg(., i = ~ Risk_Current == 0,
     
     j = c('Risk_Current'), 
     bg = "grey88", part = "body") %>% 
  
  bg(., i = ~ Risk_Current > 0,
     
     j = c('Risk_Current'), 
     bg = "skyblue3", part = "body") %>% 
  
  bg(., i = ~ Risk_Current > 3,
     
     j = c('Risk_Current'), 
     bg = "dodgerblue2", part = "body") %>% 
  
  bg(., i = ~ Risk_Current < 0,
     
     j = c('Risk_Current'), 
     bg = "darkorange1", part = "body") %>% 
  
  ## Colour the cells for 20% Risks ----
  bg(., i = ~ Risk_20pc == 0,
     
     j = c('Risk_20pc'), 
     bg = "grey88", part = "body") %>% 
  
  bg(., i = ~ Risk_20pc > 0,
     
     j = c('Risk_20pc'), 
     bg = "skyblue3", part = "body") %>% 
  
  bg(., i = ~ Risk_20pc > 3,
     
     j = c('Risk_20pc'), 
     bg = "dodgerblue2", part = "body") %>% 
  
  bg(., i = ~ Risk_20pc < 0,
     
     j = c('Risk_20pc'), 
     bg = "darkorange1", part = "body") %>% 
  
  bg(., i = ~ Risk_20pc < -3,
     
     j = c('Risk_20pc'), 
     bg = "firebrick1", part = "body") %>% 
  
  ## Colour the cells for 30% Risks ----
  bg(., i = ~ Risk_30pc == 0,
     
     j = c('Risk_30pc'), 
     bg = "grey88", part = "body") %>% 
  
  bg(., i = ~ Risk_30pc > 0,
     
     j = c('Risk_30pc'), 
     bg = "skyblue3", part = "body") %>% 
  
  bg(., i = ~ Risk_30pc > 3,
     
     j = c('Risk_30pc'), 
     bg = "dodgerblue2", part = "body") %>% 
  
  bg(., i = ~ Risk_30pc < 0,
     
     j = c('Risk_30pc'), 
     bg = "darkorange1", part = "body") %>% 
  
  bg(., i = ~ Risk_30pc < -3,
     
     j = c('Risk_30pc'), 
     bg = "firebrick1", part = "body") %>% 
  
  
  ## Colour the cells for 40% Risks ----
  bg(., i = ~ Risk_40pc == 0,
     
     j = c('Risk_40pc'), 
     bg = "grey88", part = "body") %>% 
  
  bg(., i = ~ Risk_40pc > 0,
     
     j = c('Risk_40pc'), 
     bg = "skyblue3", part = "body") %>% 
  
  
  bg(., i = ~ Risk_40pc > 3,
     
     j = c('Risk_40pc'), 
     bg = "dodgerblue2", part = "body") %>% 
  
  
  bg(., i = ~ Risk_40pc < 0,
     
     j = c('Risk_40pc'), 
     bg = "darkorange1", part = "body") %>% 
  
  bg(., i = ~ Risk_40pc < -3,
     
     j = c('Risk_40pc'), 
     bg = "firebrick1", part = "body") %>% 
  
  ## Colour the cells for 50% Risks ----
  bg(., i = ~ Risk_50pc == 0,
     
     j = c('Risk_50pc'), 
     bg = "grey88", part = "body") %>% 
  
  bg(., i = ~ Risk_50pc > 0,
     
     j = c('Risk_50pc'), 
     bg = "skyblue3", part = "body") %>% 
  
  bg(., i = ~ Risk_50pc > 3,
     
     j = c('Risk_50pc'), 
     bg = "dodgerblue2", part = "body") %>% 
  
  bg(., i = ~ Risk_50pc < 0,
     
     j = c('Risk_50pc'), 
     bg = "darkorange1", part = "body") %>% 
  
  bg(., i = ~ Risk_50pc < -3,
     
     j = c('Risk_50pc'), 
     bg = "firebrick1", part = "body") %>% 
  
  
  ## Fontsize, color and bold 
  fontsize(size = 14,  part = "header") %>%     
  fontsize(size = 10,  part = "body")  %>% 
  bold(i = 1,  j = NULL, bold = TRUE, part = "header")  %>%
  
  ## Set the colour and width
  height_all(., height = 10) %>% 
  set_table_properties(layout = "autofit")
  




# STEP 2 :: create WS Flex tables ----


##
## Create numbers for the table
WS_vals <- NSW_CHR_Water_Sources_enviro_profile %>% 
  filter(CHR_Water_Source == WS_test[1])


WS_area   <- WS_vals$Sq_km %>% round()
WS_rain   <- WS_vals$median.Annual_precip %>% round()
WS_forest <- WS_vals$max.Forest_Cov       %>% round()


## 
WS_summary_tibble <- tibble(Characteristics =
                               
                               ## Create headline numbers 
                               c("Catchment Size (km2)",
                                 "Annual Precipitation (mm)",
                                 "Current HR Uptake (ML)",
                                 "Forest Cover",
                                 "Water Dependent Assets"),
                             
                             ## Enter headline values
                             Values =
                               
                               c(WS_area,
                                 WS_rain,
                                 15,
                                 WS_forest,
                                 "Wetlands 1/2/3")) %>% 
  
  ## Turn into flex-table
  flextable() %>% 
  
  ## Set background formatting for flex-table
  bg(bg = "springgreen3", part = "header") %>% 
  bg(bg = "wheat",        part = "body")   %>%
  
  ## Fontsize, color and bold 
  fontsize(size = 14,    part = "header") %>%     
  fontsize(size = 10,    part = "body")   %>% 
  bold(i = 1,  j = NULL, bold = TRUE, part = "header") %>%
  
  ## Set the colour and width
  # color(color = "white", i=~ Values == "Count") %>%
  # color(color = "white", part = "all")          %>% 
  
  width(j =~ Characteristics,  width = 100) %>%
  width(j =~ Values,           width = 30) %>%
  height_all(., height = 10)







# STEP 3 :: create WS Flex tables ----



# STEP 3 :: create WS Flex tables ----
# if(save_environment) {
# save.image('CHR_test_run.RData')
# }


#####################################################################################################################
#############################################  ------ TBC ---- ######################################################
#####################################################################################################################
