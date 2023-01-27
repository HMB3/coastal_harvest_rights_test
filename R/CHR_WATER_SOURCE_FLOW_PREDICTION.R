#########################################################################################################################
###############################  ------ HARC MODEL INFERENCE ---- #######################################################
#########################################################################################################################


# \ 
# 
# This coom the HARC Models to all the othe  water sources
#   
#   \de predicts flow from metrics fr


## look at plots in detail?
write_gam_results <- TRUE





## FLOW + ENVIRO DATA =============================================================


## 
if(harc_predict) {
  
  
  ## Decision Criteria master table
  Flow_variables_master <- 
    
    read_excel_allsheets(paste0(hydro_data,
                                'HARC/HARC_Percentage_Change_Flow_v2.xlsx'))
  
  
  Flow_calculations <- Flow_variables_master$`dQ area Calculations_petter_GAM` %>%
    mutate(CHR_Water_Source = str_trim(CHR_Water_Source),
           GhostName        = str_trim(GhostName))
  
  
  ## LUT that does the join
  HARC_WS_LUT <- Flow_variables_master$HATC_WS_LUT
  harc_chr_ws <- HARC_WS_LUT$CHR_Water_Source %>% unique()
  
  
  
  CHR_sources_zonal_stats_table <- 
    
    read_csv(paste0(enviro_out, 'NSW_CHR_Water_Sources_enviro_profile.csv')) %>% 
    
    rename(Harc_Hectares             = Hectares,
           Harc_Sq_km                = Sq_km,
           Harc_Stream_length_km     = Stream_length_km,
           Harc_Stream_density_sqkm  = Stream_density_sqkm) %>% 
    
    ## This one is a 'sliver' from a aggregation/intersection
    ## All the other high stream densities are genuine - very small WSs
    filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')
  
  
  CHR_sources_areas_harc <- CHR_sources_zonal_stats_table %>% 
    select(CHR_Water_Source, Harc_Sq_km) %>% 
    .[.$CHR_Water_Source %in% harc_chr_ws, ]
  
  
  
  NSW_CHR_HARC_catch_profile <- 
    
    read_csv(paste0(enviro_out, 'NSW_CHR_HARC_enviro_profile.csv')) %>% 
    
    select(-contains("Lithology"))  %>% 
    select(-contains("LUE"))        %>% 
    select(-contains("ASC"))        %>% 
    select(-contains("ConArea"))    %>%
    select(-contains("Forest_Cov")) %>% 
    
    ## 
    inner_join(., HARC_WS_LUT, by = "GhostName")       %>% 
    select(-MAJOR_CATC, -HARC_Catchment)               %>% 
    select(CHR_Water_Source,  GhostName, everything()) %>% 
    mutate(CHR_Water_Source = 
             ifelse(GhostName == 'Nambucca', 
                    'Coastal Nambucca River Water Source', CHR_Water_Source))
  
  write_csv(NSW_CHR_HARC_catch_profile, paste0(data_dir, 'HARC_Join_CHR_catch_profile.csv'))
  
  
  ## Get the distinct values according to GhostName
  NSW_CHR_HARC_catch_profile_distinct <- NSW_CHR_HARC_catch_profile %>% 
    
    distinct(GhostName, .keep_all = TRUE) %>% 
    mutate(CHR_Water_Source = str_trim(CHR_Water_Source),
           GhostName        = str_trim(GhostName))
  
  
  ## Don't use the areas - these have already been factored into the flow variables
  enviro_vars <- names(NSW_CHR_HARC_catch_profile)[3:ncol(NSW_CHR_HARC_catch_profile)][-3][-3]
  
  
  setdiff(NSW_CHR_HARC_catch_profile$GhostName, Flow_calculations$GhostName)
  setdiff(NSW_CHR_HARC_catch_profile$CHR_Water_Source, Flow_calculations$CHR_Water_Source)
  
  
  ## Calculate the % WS area available for farm dams ----
  CHR_UWS_farm_dams_area_volume_contours_df <- 
    
    read_csv(paste0(hydro_out, 
                    'dams/', 
                    'NSW_CHR_Water_Sources_Farm_dams_areas.csv'))
  
  
  CHR_farm_dams_area_volume <- CHR_UWS_farm_dams_area_volume_contours_df %>% 
    
    group_by(CHR_Water_Source) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  
  
  ## Now join on the data for the areas
  NSW_CHR_Water_Source_Area <- 
    
    st_read(dsn = paste0(data_dir, 
                         'CHR_DPEW_Input_Water_Source_Data.gpkg'),
            layer = 'NSW_CHR_Water_Sources_aggregated') %>% 
    as_tibble() %>% 
    select(-geom)
  
  
  NSW_CHR_LUT <- st_read(dsn = paste0(data_dir, 
                                      'CHR_DPEW_Input_Water_Source_Data.gpkg'),
                         layer = 'NSW_unreg_coastal_water_sources') %>% 
    as_tibble() %>% 
    select(MAJOR_CATCH,
           EXTRACTION_MANAGEMENT_UNIT,
           UNREGULATED_WATER_SOURCE, 
           CHR_Water_Source,
           W_Source_ID, 
           Est_No1) %>% distinct()
  
  
  NSW_CATCH_CHR_LUT <- NSW_CHR_LUT %>% 
    
    select(MAJOR_CATCH,
           EXTRACTION_MANAGEMENT_UNIT,
           CHR_Water_Source) %>% distinct()
  
  
  ## Combine farm dam area with actual area
  ## Volume_ML / hectares = Volume_ML_per_Ha
  ## Uptake = (Volume_ML_per_Ha / contour median) * 100
  CHR_farm_dams_area_volume_percent <- CHR_farm_dams_area_volume %>%
    
    inner_join(., NSW_CHR_Water_Source_Area, by = "CHR_Water_Source") %>% 
    inner_join(., NSW_CATCH_CHR_LUT,         by = "CHR_Water_Source") %>% 
    distinct(CHR_Water_Source, .keep_all = TRUE) %>% 
    
    ## Note that all the % areas look right, except Sandy Creek, which seems to 
    ## be inverted - 20% when it should be 80.
    mutate(Eligible_Area = (Dam_Sqkm/CHR_Sqkm) * 100 %>% round(), 
           Eligible_Area = formatC(Eligible_Area, format = "f", digits = 2) %>% as.numeric(),
           Eligible_Area = ifelse(Eligible_Area > 100, 100, Eligible_Area),
           
           Volume_ML_per_Ha = Volume_ML/Dam_Ha,
           Uptake_prop      = (Volume_ML_per_Ha / Contour_median),
           Uptake_per       = (Volume_ML_per_Ha / Contour_median) *100) %>%
    
    select(MAJOR_CATCH,
           EXTRACTION_MANAGEMENT_UNIT,
           CHR_Water_Source,
           everything())
  
  ## Save out
  write_csv(CHR_farm_dams_area_volume_percent, 
            paste0(hydro_out, 'dams/', 
                   'CHR_farm_dams_area_volume_percent.csv'))
  
  
  CHR_farm_dams_elligble <- CHR_farm_dams_area_volume_percent #%>% 
  
  # select(MAJOR_CATCH, 
  #        EXTRACTION_MANAGEMENT_UNIT, 
  #        CHR_Water_Source, 
  #        Eligible_Area)
  
  
  ## Join the eligible area to the 
  CHR_sources_zonal_stats_table_eligible <- 
    
    CHR_sources_zonal_stats_table %>% 
    left_join(., CHR_farm_dams_elligble, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH,
           EXTRACTION_MANAGEMENT_UNIT,
           CHR_Water_Source,
           Eligible_Area,
           Harc_Hectares, 
           Harc_Sq_km, 
           Harc_Stream_length_km, 
           Harc_Stream_density_sqkm,
           everything()) %>%
    
    filter(!is.na(Eligible_Area))
  
  
  ## 
  Flow_DQ_area_calcs_enviro <- Flow_calculations %>% 
    
    left_join(., NSW_CHR_HARC_catch_profile_distinct, 
              by = c("CHR_Water_Source", "GhostName")) %>% 
    
    ## NA flow percentiles are actually 0, no flow
    mutate_all(~replace(., is.na(.), 0)) 
  
  setdiff(unique(NSW_CHR_HARC_catch_profile$CHR_Water_Source), 
          unique(Flow_DQ_area_calcs_enviro$CHR_Water_Source))
  setdiff(names(CHR_sources_zonal_stats_table), names(Flow_DQ_area_calcs_enviro))
  
  
  ## Save out
  write_csv(Flow_DQ_area_calcs_enviro, paste0(data_dir, 'HARC_Flow_DQ_enviro_stats.csv'))
  
  
  
  
  
  ## Update here
  HARC_flow_measures_10 <- Flow_DQ_area_calcs_enviro %>% 
    filter(Harv_Limit == 10) 
  
  HARC_flow_measures_20 <- Flow_DQ_area_calcs_enviro %>% 
    filter(Harv_Limit == 20)
  
  HARC_flow_measures_30 <- Flow_DQ_area_calcs_enviro %>% 
    filter(Harv_Limit == 30)
  
  HARC_flow_measures_40 <- Flow_DQ_area_calcs_enviro %>% 
    filter(Harv_Limit == 40)
  
  HARC_flow_measures_50 <- Flow_DQ_area_calcs_enviro %>% 
    filter(Harv_Limit == 50)
  
  
  
  # STEP 2 :: run ordination ----
  
  
  if(ordination) {
    source('./R/CHR_ORDINATIONS_PLOTS.R')
  }
  
  
  
  
  
  # STEP 1 :: Variable selection  ----
  
  
  ## For the correlation matrix, pick a range of variables
  ## 98%, 88%, for just 40-50% Dams that make a difference
  
  
  ## Correlation variables
  ## get the strongest correlations with the explanatory variables
  ## Create a correlation matrix for all the varSoory Iiables.
  ## Not as reliable as non-linear, but still useful
  ##  Turn this into a function...
  
  
  ## here we pick a few of the bins - a few high, a few low
  ## 94%-ile, 84, 75, 50
  pairwise_correlations_10_50 <- pairwise_correlations(correlationn_df = HARC_flow_measures_10,
                                                       respose         = 'pc50th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim10_')
  
  
  pairwise_correlations_10_75 <- pairwise_correlations(correlationn_df = HARC_flow_measures_10,
                                                       respose         = 'pc75th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim10_')
  
  
  pairwise_correlations_10_84 <- pairwise_correlations(correlationn_df = HARC_flow_measures_10,
                                                       respose         = 'pc84th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim10_')
  
  
  pairwise_correlations_10_94 <- pairwise_correlations(correlationn_df = HARC_flow_measures_10,
                                                       respose         = 'pc94th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim10_')
  
  
  
  pairwise_correlations_30_50 <- pairwise_correlations(correlationn_df = HARC_flow_measures_30,
                                                       respose         = 'pc50th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim30_')
  
  
  pairwise_correlations_30_75 <- pairwise_correlations(correlationn_df = HARC_flow_measures_30,
                                                       respose         = 'pc75th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim30_')
  
  
  pairwise_correlations_30_84 <- pairwise_correlations(correlationn_df = HARC_flow_measures_30,
                                                       respose         = 'pc84th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim30_')
  
  
  pairwise_correlations_30_94 <- pairwise_correlations(correlationn_df = HARC_flow_measures_30,
                                                       respose         = 'pc94th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim30_')
  
  
  pairwise_correlations_50_50 <- pairwise_correlations(correlationn_df = HARC_flow_measures_50,
                                                       respose         = 'pc50th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim50_')
  
  
  pairwise_correlations_50_75 <- pairwise_correlations(correlationn_df = HARC_flow_measures_50,
                                                       respose         = 'pc75th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim50_')
  
  
  pairwise_correlations_50_84 <- pairwise_correlations(correlationn_df = HARC_flow_measures_50,
                                                       respose         = 'pc84th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim50_')
  
  pairwise_correlations_50_94 <- pairwise_correlations(correlationn_df = HARC_flow_measures_50,
                                                       respose         = 'pc94th',
                                                       vars            = enviro_vars,
                                                       lim             = 'lim50_')
  
  
  ## Bind them all together into a data frame
  Correlation_summary <- bind_rows(pairwise_correlations_10_50[[1]],
                                   pairwise_correlations_10_50[[2]],
                                   
                                   pairwise_correlations_10_75[[1]],
                                   pairwise_correlations_10_75[[2]],
                                   
                                   pairwise_correlations_10_84[[1]],
                                   pairwise_correlations_10_84[[2]],
                                   
                                   pairwise_correlations_10_94[[1]],
                                   pairwise_correlations_10_94[[2]],
                                   
                                   
                                   pairwise_correlations_30_50[[1]],
                                   pairwise_correlations_30_50[[2]],
                                   
                                   pairwise_correlations_30_75[[1]],
                                   pairwise_correlations_30_75[[2]],
                                   
                                   pairwise_correlations_30_84[[1]],
                                   pairwise_correlations_30_84[[2]],
                                   
                                   pairwise_correlations_30_94[[1]],
                                   pairwise_correlations_30_94[[2]],
                                   
                                   
                                   pairwise_correlations_50_50[[1]],
                                   pairwise_correlations_50_50[[2]],
                                   
                                   pairwise_correlations_50_75[[1]],
                                   pairwise_correlations_50_75[[2]],
                                   
                                   pairwise_correlations_50_84[[1]],
                                   pairwise_correlations_50_84[[2]],
                                   
                                   pairwise_correlations_50_94[[1]],
                                   pairwise_correlations_50_94[[2]])
  
  
  ## Check ou the results
  View(Correlation_summary)
  write_csv(Correlation_summary, paste0(hydro_out, '/harc_models/flow_Correlation_summary.csv'))
  
  
  
  ## Just plot each source against each variable
  ## Hectares, Med_rain_ann_5km, Stream_density,
  ## Med_Slope, Max_AWC, Max_SND, Med_Forest_Cov
  ## Do the scatterplot matrix on the subset of enviro variables that are correlated with lower
  ## %tile flow duration bins -
  
  
  ## Plot env correlations ----
  png(paste0(hydro_out, '/harc_models/Climate_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_10 %>%
                        
                        ## This is subjective
                        dplyr::select(median.Precip_dry_month,
                                      median.Precip_wet_month,
                                      median.PTS1,
                                      median.EPX,
                                      median.WDX,
                                      median.Annual_mean_temp,
                                      median.Max_temp_warm_month),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  png(paste0(hydro_out, '/harc_models/Topo_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_10 %>%
                        
                        ## This is subjective
                        dplyr::select(median.Topo,
                                      median.Elevation,
                                      median.PROFCURV,
                                      median.PlanCurv,
                                      median.SlopeDeg,
                                      median.TWI),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  png(paste0(hydro_out, '/harc_models/Soil_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_10 %>%
                        
                        ## This is subjective
                        dplyr::select(median.AWC,
                                      median.BDW,
                                      median.SND,
                                      median.SLT,
                                      median.CLY,
                                      median.DES,
                                      median.DER),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  png(paste0(hydro_out, '/harc_models/Stream_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_10 %>%
                        
                        ## This is subjective
                        dplyr::select('Eligible Area (%)',
                                      Harc_Stream_length_km,
                                      Harc_Stream_density_sqkm,
                                      median.Elevation),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  ## Plot 50% figures 
  png(paste0(hydro_out, '/harc_models/flow_40lim_50pc_env_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_10 %>%
                        
                        ## This is subjective
                        dplyr::select("pc50th",
                                      all_of(pairwise_correlations_10_50[[3]] %>% na.omit())),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  ## Plot 75% figures 
  png(paste0(hydro_out, '/harc_models/flow_40lim_94pc_env_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_10 %>%
                        
                        ## This is subjective
                        dplyr::select("pc94th",
                                      all_of(pairwise_correlations_10_94[[3]] %>% na.omit())),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  ## Plot 50% figures 
  png(paste0(hydro_out, '/harc_models/flow_30lim_50pc_env_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_30 %>%
                        
                        ## This is subjective
                        dplyr::select("pc50th",
                                      all_of(pairwise_correlations_30_50[[3]] %>% na.omit())),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  ## Plot 75% figures 
  png(paste0(hydro_out, '/harc_models/flow_30lim_94pc_env_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_30 %>%
                        
                        ## This is subjective
                        dplyr::select("pc94th",
                                      all_of(pairwise_correlations_30_94[[3]] %>% na.omit())),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  
  ## Plot 50% figures 
  png(paste0(hydro_out, '/harc_models/flow_50lim_50pc_env_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_50 %>%
                        
                        ## This is subjective
                        dplyr::select("pc50th",
                                      all_of(pairwise_correlations_50_50[[3]] %>% na.omit())),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  ## Plot 75% figures 
  png(paste0(hydro_out, 'harc_models/flow_50lim_94pc_env_vars_corr.png'),
      16, 10, units = 'in', res = 400)
  
  psych::pairs.panels(HARC_flow_measures_50 %>%
                        
                        ## This is subjective
                        dplyr::select("pc94th",
                                      all_of(pairwise_correlations_50_94[[3]] %>% na.omit())),
                      
                      smooth     = FALSE,
                      method     = "pearson", # correlation method
                      hist.col   = "#00AFBB",
                      density    = TRUE,      # show density plots
                      ellipses   = FALSE,
                      cex        = 1.8,
                      cex.cor    = 1.2,
                      cex.labels = 1.6,
                      lwd        = 2,
                      col        = "blue")
  
  dev.off()
  
  
  ## Histograms of the variables used in the models
  stream_hist <- histogram(CHR_sources_zonal_stats_table$Harc_Stream_length_km)
  
  
  bd_hist     <- histogram(CHR_sources_zonal_stats_table$min.BDW,
                           xlab = 'min Bulk Density')  
  
  epx_hist    <- histogram(CHR_sources_zonal_stats_table$min.EPX,
                           xlab = 'min EPX') 
  
  temp_hist   <- histogram(CHR_sources_zonal_stats_table$min.Max_temp_warm_month,
                           xlab = 'Max temp warm month')
  
  
  ## Plot 75% figures 
  png(paste0(hydro_out, 'harc_models/gam_xvar_histograms.png'),
      16, 10, units = 'in', res = 400)
  
  cowplot::plot_grid(stream_hist,
                     bd_hist,
                     epx_hist,
                     temp_hist, ncol = 2)
  
  dev.off()
  
  
  # STEP 3 :: run GAMS ----
  
  
  ## Check the model
  # First, gam.check() reports on model convergence. Here, it reports full convergence. 
  # R has found a best solution. If the model has not converged, results are likely not correct. 
  # This can happen when there are too many parameters in the model for not enough data.
  # 
  # Below, we see a table of basis checking results. This shows a statistical test for patterns 
  # in model residuals, which should be random. Each line reports the test results for one smooth. 
  # It shows the k value or number of basis functions, the effective degrees of freedom, a test 
  # statistic, and p-value.
  # 
  # Here, small p-values indicate that residuals are not randomly distributed. This often means 
  # there are not enough basis functions
  
  
  ## Full set : BDW, EPX, Harc_Stream_density_sqkm, min.Max_temp_warm_month
  
  # xvars  = c('s(min.BDW,                  k=3) + 
  #             s(max.EPX,                  k=3) + 
  #             s(Harc_Stream_density_sqkm, k=3) +
  #             s(min.Max_temp_warm_month,  k=3)'),
  
  # xvars  = c('s(min.BDW,                  k=3) +
  #             s(Harc_Stream_density_sqkm, k=3) +
  #             s(min.EPX,                  k=3)')
  
  ## No climate : BDW, Harc_Stream_density_sqkm 
  
  ## Run Multiple GAMs ----
  flow_metrics_10lim_enviro_gams <- gam_formula_analyses(yvars  = c('pc10th ~ ',
                                                                    'pc20th ~ ',
                                                                    'pc30th ~ ',
                                                                    'pc40th ~ ',
                                                                    'pc50th ~ ',
                                                                    
                                                                    'pc60th ~ ',
                                                                    'pc65th ~ ',
                                                                    'pc70th ~ ',
                                                                    'pc75th ~ ',
                                                                    
                                                                    'pc80th ~ ',
                                                                    'pc82nd ~ ',
                                                                    'pc84th ~ ',
                                                                    'pc86th ~ ',
                                                                    'pc88th ~ ',
                                                                    
                                                                    'pc90th ~ ',
                                                                    'pc92nd ~ ',
                                                                    'pc94th ~ ',
                                                                    'pc96th ~ ',
                                                                    'pc98th ~ ',
                                                                    'pc100th ~ '),
                                                         
                                                         lim    = 10, 
                                                         
                                                         xvars  = c('s(min.BDW,                  k=3) +
                                                                     s(Harc_Stream_density_sqkm, k=3)'),
                                                         
                                                         model_data     = HARC_flow_measures_10,
                                                         sel_method     = 'REML',
                                                         predict_data   = CHR_sources_zonal_stats_table_eligible,
                                                         out_dir        = paste0(hydro_out, 'harc_models/'))
  
  
  flow_metrics_20lim_enviro_gams <- gam_formula_analyses(yvars  = c('pc10th ~ ',
                                                                    'pc20th ~ ',
                                                                    'pc30th ~ ',
                                                                    'pc40th ~ ',
                                                                    'pc50th ~ ',
                                                                    
                                                                    'pc60th ~ ',
                                                                    'pc65th ~ ',
                                                                    'pc70th ~ ',
                                                                    'pc75th ~ ',
                                                                    
                                                                    'pc80th ~ ',
                                                                    'pc82nd ~ ',
                                                                    'pc84th ~ ',
                                                                    'pc86th ~ ',
                                                                    'pc88th ~ ',
                                                                    
                                                                    'pc90th ~ ',
                                                                    'pc92nd ~ ',
                                                                    'pc94th ~ ',
                                                                    'pc96th ~ ',
                                                                    'pc98th ~ ',
                                                                    'pc100th ~ '),
                                                         
                                                         lim    = 20, 
                                                         
                                                         xvars  = c('s(min.BDW,                  k=3) +
                                                                     s(Harc_Stream_density_sqkm, k=3)'),
                                                         
                                                         model_data     = HARC_flow_measures_20,
                                                         sel_method     = 'REML',
                                                         predict_data   = CHR_sources_zonal_stats_table_eligible,
                                                         out_dir        = paste0(hydro_out, 'harc_models/'))
  
  
  flow_metrics_30lim_enviro_gams <- gam_formula_analyses(yvars  = c('pc10th ~ ',
                                                                    'pc20th ~ ',
                                                                    'pc30th ~ ',
                                                                    'pc40th ~ ',
                                                                    'pc50th ~ ',
                                                                    
                                                                    'pc60th ~ ',
                                                                    'pc65th ~ ',
                                                                    'pc70th ~ ',
                                                                    'pc75th ~ ',
                                                                    
                                                                    'pc80th ~ ',
                                                                    'pc82nd ~ ',
                                                                    'pc84th ~ ',
                                                                    'pc86th ~ ',
                                                                    'pc88th ~ ',
                                                                    
                                                                    'pc90th ~ ',
                                                                    'pc92nd ~ ',
                                                                    'pc94th ~ ',
                                                                    'pc96th ~ ',
                                                                    'pc98th ~ ',
                                                                    'pc100th ~ '),
                                                         
                                                         lim    = 30, 
                                                         
                                                         xvars  = c('s(min.BDW,                  k=3) +
                                                                     s(Harc_Stream_density_sqkm, k=3)'),
                                                         
                                                         
                                                         model_data     = HARC_flow_measures_30,
                                                         sel_method     = 'REML',
                                                         predict_data   = CHR_sources_zonal_stats_table_eligible,
                                                         out_dir        = paste0(hydro_out, 'harc_models/'))
  
  
  
  flow_metrics_40lim_enviro_gams <- gam_formula_analyses(yvars  = c('pc10th ~ ',
                                                                    'pc20th ~ ',
                                                                    'pc30th ~ ',
                                                                    'pc40th ~ ',
                                                                    'pc50th ~ ',
                                                                    
                                                                    'pc60th ~ ',
                                                                    'pc65th ~ ',
                                                                    'pc70th ~ ',
                                                                    'pc75th ~ ',
                                                                    
                                                                    'pc80th ~ ',
                                                                    'pc82nd ~ ',
                                                                    'pc84th ~ ',
                                                                    'pc86th ~ ',
                                                                    'pc88th ~ ',
                                                                    
                                                                    'pc90th ~ ',
                                                                    'pc92nd ~ ',
                                                                    'pc94th ~ ',
                                                                    'pc96th ~ ',
                                                                    'pc98th ~ ',
                                                                    'pc100th ~ '),
                                                         
                                                         lim    = 40, 
                                                         
                                                         xvars  = c('s(min.BDW,                  k=3) +
                                                                     s(Harc_Stream_density_sqkm, k=3)'),
                                                         
                                                         
                                                         model_data     = HARC_flow_measures_40,
                                                         sel_method     = 'REML',
                                                         predict_data   = CHR_sources_zonal_stats_table_eligible,
                                                         out_dir        = paste0(hydro_out, 'harc_models/'))
  
  
  flow_metrics_50lim_enviro_gams <- gam_formula_analyses(yvars  = c('pc10th ~ ',
                                                                    'pc20th ~ ',
                                                                    'pc30th ~ ',
                                                                    'pc40th ~ ',
                                                                    'pc50th ~ ',
                                                                    
                                                                    'pc60th ~ ',
                                                                    'pc65th ~ ',
                                                                    'pc70th ~ ',
                                                                    'pc75th ~ ',
                                                                    
                                                                    'pc80th ~ ',
                                                                    'pc82nd ~ ',
                                                                    'pc84th ~ ',
                                                                    'pc86th ~ ',
                                                                    'pc88th ~ ',
                                                                    
                                                                    'pc90th ~ ',
                                                                    'pc92nd ~ ',
                                                                    'pc94th ~ ',
                                                                    'pc96th ~ ',
                                                                    'pc98th ~ ',
                                                                    'pc100th ~ '),
                                                         
                                                         lim    = 50, 
                                                         
                                                         xvars  = c('s(min.BDW,                  k=3) +
                                                                     s(Harc_Stream_density_sqkm, k=3)'),
                                                         
                                                         
                                                         model_data     = HARC_flow_measures_50,
                                                         sel_method     = 'REML',
                                                         predict_data   = CHR_sources_zonal_stats_table_eligible,
                                                         out_dir        = paste0(hydro_out, 'harc_models/'))
  
  
  
  
  ## Aggregate GAM Results ----
  CHR_sources_eligible_GAMs <- 
    
    CHR_sources_zonal_stats_table_eligible %>% 
    
    ## Get the key columns we might need
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope)
  
  
  flow_pred_10th <- bind_cols(flow_metrics_10lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_CI"]][1:3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]][3],
                              
                              flow_metrics_10lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3],
                              
                              flow_metrics_10lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_CI"]][3],
                              
                              flow_metrics_10lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_CI"]][3],
                              flow_metrics_10lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_CI"]][3]) %>% 
    
    left_join(., CHR_sources_eligible_GAMs, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope, 
           
           everything())
  
  
  
  
  
  flow_harc_10lim <- HARC_flow_measures_10[1:32] %>% bind_cols(., HARC_flow_measures_10['min.BDW'],
                                                               HARC_flow_measures_10['median.Elevation'],
                                                               HARC_flow_measures_10['median.Slope']) %>% 
    
    bind_cols(flow_metrics_20lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_harc"]],
              
              flow_metrics_20lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_harc"]],
              
              flow_metrics_20lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_harc"]],
              
              flow_metrics_20lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_harc"]]) %>% 
    
    select("HARC Report Name", 
           CHR_Water_Source, 
           GhostName,
           Harc_Stream_density_sqkm,
           Harc_Hectares,
           Harc_Sq_km,
           "Eligible Area (%)",
           Uptake,
           min.BDW,
           median.Elevation,
           median.Slope,
           median.Annual_mean_temp,
           median.Annual_precip,
           
           pc10th, pc10th_gam,
           pc20th, pc20th_gam,
           pc30th, pc30th_gam,
           pc40th, pc40th_gam,
           pc50th, pc50th_gam,
           pc60th, pc60th_gam,
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc82nd, pc82nd_gam,
           pc84th, pc84th_gam,
           pc86th, pc86th_gam,
           pc88th, pc88th_gam,
           pc90th, pc90th_gam,
           
           pc92nd, pc92nd_gam,
           pc94th, pc94th_gam,
           pc96th, pc96th_gam,
           pc98th, pc98th_gam,
           pc100th, pc100th_gam)
  
  
  ##
  write_csv(flow_harc_10th, paste0(hydro_out, '/harc_models/flow_harc_10lim.csv'))
  
  
  
  flow_pred_10th_full <- bind_cols(flow_metrics_40lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3:6]) %>% 
    
    left_join(., CHR_sources_eligible_GAMs, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope, 
           
           everything())
  
  
  flow_pred_20th <- bind_cols(flow_metrics_20lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_CI"]][1:3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]][3],
                              
                              flow_metrics_20lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3],
                              
                              flow_metrics_20lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_CI"]][3],
                              
                              flow_metrics_20lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_CI"]][3],
                              flow_metrics_20lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_CI"]][3]) %>% 
    
    left_join(., CHR_sources_eligible_GAMs, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope, 
           
           everything())
  
  
  
  flow_harc_20lim <- HARC_flow_measures_20[1:32] %>% bind_cols(., 
                                                               HARC_flow_measures_20['min.BDW'],
                                                               HARC_flow_measures_20['median.Elevation'],
                                                               HARC_flow_measures_20['median.Slope']) %>% 
    
    bind_cols(flow_metrics_20lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_harc"]],
              
              flow_metrics_20lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_harc"]],
              
              flow_metrics_20lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_harc"]],
              
              flow_metrics_20lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_harc"]],
              flow_metrics_20lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_harc"]]) %>% 
    
    select("HARC Report Name", 
           CHR_Water_Source, 
           GhostName,
           Harc_Stream_density_sqkm,
           Harc_Hectares,
           Harc_Sq_km,
           "Eligible Area (%)",
           Uptake,
           min.BDW,
           median.Elevation,
           median.Slope,
           median.Annual_mean_temp,
           median.Annual_precip,
           
           pc10th, pc10th_gam,
           pc20th, pc20th_gam,
           pc30th, pc30th_gam,
           pc40th, pc40th_gam,
           pc50th, pc50th_gam,
           pc60th, pc60th_gam,
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc82nd, pc82nd_gam,
           pc84th, pc84th_gam,
           pc86th, pc86th_gam,
           pc88th, pc88th_gam,
           pc90th, pc90th_gam,
           
           pc92nd, pc92nd_gam,
           pc94th, pc94th_gam,
           pc96th, pc96th_gam,
           pc98th, pc98th_gam,
           pc100th, pc100th_gam)
  
  
  ##
  write_csv(flow_harc_20lim, paste0(hydro_out, '/harc_models/flow_harc_20lim.csv'))
  
  
  
  
  
  flow_pred_20th_full <- bind_cols(flow_metrics_20lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]],
                                   CHR_sources_zonal_stats_table_eligible['Harc_Stream_density_sqkm'],
                                   CHR_sources_zonal_stats_table_eligible['min.BDW'],
                                   CHR_sources_zonal_stats_table_eligible['min.EPX'],
                                   
                                   flow_metrics_20lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_20lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_20lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3:6])
  
  
  flow_pred_30th      <- bind_cols(flow_metrics_30lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_CI"]][1:3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_30lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_30lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_30lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_CI"]][3]) %>% 
    
    left_join(., CHR_sources_eligible_GAMs, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope, 
           
           everything())
  
  
  
  flow_harc_30lim <- HARC_flow_measures_30[1:32] %>% bind_cols(., 
                                                               HARC_flow_measures_30['min.BDW'],
                                                               HARC_flow_measures_30['median.Elevation'],
                                                               HARC_flow_measures_30['median.Slope']) %>% 
    
    bind_cols(flow_metrics_30lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_harc"]],
              
              flow_metrics_30lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_harc"]],
              
              flow_metrics_30lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_harc"]],
              
              flow_metrics_30lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_harc"]],
              flow_metrics_30lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_harc"]]) %>% 
    
    select("HARC Report Name", 
           CHR_Water_Source, 
           GhostName,
           Harc_Stream_density_sqkm,
           Harc_Hectares,
           Harc_Sq_km,
           "Eligible Area (%)",
           Uptake,
           min.BDW,
           median.Elevation,
           median.Slope,
           median.Annual_mean_temp,
           median.Annual_precip,
           
           pc10th, pc10th_gam,
           pc20th, pc20th_gam,
           pc30th, pc30th_gam,
           pc40th, pc40th_gam,
           pc50th, pc50th_gam,
           pc60th, pc60th_gam,
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc82nd, pc82nd_gam,
           pc84th, pc84th_gam,
           pc86th, pc86th_gam,
           pc88th, pc88th_gam,
           pc90th, pc90th_gam,
           
           pc92nd, pc92nd_gam,
           pc94th, pc94th_gam,
           pc96th, pc96th_gam,
           pc98th, pc98th_gam,
           pc100th, pc100th_gam)
  
  
  ##
  write_csv(flow_harc_30lim, paste0(hydro_out, '/harc_models/flow_harc_30lim.csv'))
  
  
  
  
  flow_pred_30th_full <- bind_cols(flow_metrics_30lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]],
                                   CHR_sources_zonal_stats_table_eligible['Harc_Stream_density_sqkm'],
                                   CHR_sources_zonal_stats_table_eligible['min.BDW'],
                                   CHR_sources_zonal_stats_table_eligible['min.EPX'],
                                   
                                   flow_metrics_30lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_30lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3:6])
  
  
  flow_pred_40th      <- bind_cols(flow_metrics_40lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_CI"]][1:3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_40lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_40lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_40lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_CI"]][3]) %>% 
    
    left_join(., CHR_sources_eligible_GAMs, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope, 
           
           everything())
  
  
  
  
  flow_harc_40lim <- HARC_flow_measures_40[1:32] %>% bind_cols(., 
                                                               HARC_flow_measures_40['min.BDW'],
                                                               HARC_flow_measures_40['median.Elevation'],
                                                               HARC_flow_measures_40['median.Slope']) %>% 
    
    
    bind_cols(flow_metrics_40lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_harc"]],
              
              flow_metrics_40lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_harc"]],
              
              flow_metrics_40lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_harc"]],
              
              flow_metrics_40lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_harc"]],
              flow_metrics_40lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_harc"]]) %>% 
    
    select("HARC Report Name", 
           CHR_Water_Source, 
           GhostName,
           Harc_Stream_density_sqkm,
           Harc_Hectares,
           Harc_Sq_km,
           "Eligible Area (%)",
           Uptake,
           min.BDW,
           median.Elevation,
           median.Slope,
           median.Annual_mean_temp,
           median.Annual_precip,
           
           pc10th, pc10th_gam,
           pc20th, pc20th_gam,
           pc30th, pc30th_gam,
           pc40th, pc40th_gam,
           pc50th, pc50th_gam,
           pc60th, pc60th_gam,
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc82nd, pc82nd_gam,
           pc84th, pc84th_gam,
           pc86th, pc86th_gam,
           pc88th, pc88th_gam,
           pc90th, pc90th_gam,
           
           pc92nd, pc92nd_gam,
           pc94th, pc94th_gam,
           pc96th, pc96th_gam,
           pc98th, pc98th_gam,
           pc100th, pc100th_gam)
  
  
  ##
  write_csv(flow_harc_40lim, paste0(hydro_out, '/harc_models/flow_harc_40lim.csv'))
  
  
  
  flow_pred_40th_full <- bind_cols(flow_metrics_40lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]],
                                   CHR_sources_zonal_stats_table_eligible['Harc_Stream_density_sqkm'],
                                   CHR_sources_zonal_stats_table_eligible['min.BDW'],
                                   CHR_sources_zonal_stats_table_eligible['min.EPX'],
                                   
                                   flow_metrics_40lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_40lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3:6])
  
  
  flow_pred_50th      <- bind_cols(flow_metrics_50lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_CI"]][1:3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_50lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_50lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_CI"]][3],
                                   
                                   flow_metrics_50lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_CI"]][3],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_CI"]][3]) %>% 
    
    left_join(., CHR_sources_eligible_GAMs, by = "CHR_Water_Source") %>%
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Uptake_prop,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope, 
           
           everything())
  
  
  
  flow_harc_50lim <- HARC_flow_measures_50[1:32] %>% bind_cols(., 
                                                               HARC_flow_measures_50['min.BDW'],
                                                               HARC_flow_measures_50['median.Elevation'],
                                                               HARC_flow_measures_50['median.Slope']) %>% 
    
    bind_cols(flow_metrics_50lim_enviro_gams[["GAM : pc10th ~ "]][["pc10th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc20th ~ "]][["pc20th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc30th ~ "]][["pc30th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc40th ~ "]][["pc40th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_harc"]],
              
              flow_metrics_50lim_enviro_gams[["GAM : pc60th ~ "]][["pc60th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc65th ~ "]][["pc65th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc70th ~ "]][["pc70th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_harc"]],
              
              flow_metrics_50lim_enviro_gams[["GAM : pc80th ~ "]][["pc80th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc82nd ~ "]][["pc82nd ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc86th ~ "]][["pc86th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc88th ~ "]][["pc88th ~ _GAM_predict_harc"]],
              
              flow_metrics_50lim_enviro_gams[["GAM : pc90th ~ "]][["pc90th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc92nd ~ "]][["pc92nd ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc96th ~ "]][["pc96th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc98th ~ "]][["pc98th ~ _GAM_predict_harc"]],
              flow_metrics_50lim_enviro_gams[["GAM : pc100th ~ "]][["pc100th ~ _GAM_predict_harc"]]) %>% 
    
    select("HARC Report Name", 
           CHR_Water_Source, 
           GhostName,
           Harc_Stream_density_sqkm,
           Harc_Hectares,
           Harc_Sq_km,
           "Eligible Area (%)",
           Uptake,
           min.BDW,
           median.Elevation,
           median.Slope,
           median.Annual_mean_temp,
           median.Annual_precip,
           
           pc10th, pc10th_gam,
           pc20th, pc20th_gam,
           pc30th, pc30th_gam,
           pc40th, pc40th_gam,
           pc50th, pc50th_gam,
           pc60th, pc60th_gam,
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc65th, pc65th_gam,
           pc70th, pc70th_gam,
           pc75th, pc75th_gam,
           pc80th, pc80th_gam,
           
           pc82nd, pc82nd_gam,
           pc84th, pc84th_gam,
           pc86th, pc86th_gam,
           pc88th, pc88th_gam,
           pc90th, pc90th_gam,
           
           pc92nd, pc92nd_gam,
           pc94th, pc94th_gam,
           pc96th, pc96th_gam,
           pc98th, pc98th_gam,
           pc100th, pc100th_gam)
  
  
  ##
  write_csv(flow_harc_50lim, paste0(hydro_out, '/harc_models/flow_harc_50lim.csv'))
  
  
  
  flow_pred_50th_full <- bind_cols(flow_metrics_50lim_enviro_gams[["GAM : pc50th ~ "]][["pc50th ~ _GAM_predict_CI"]],
                                   CHR_sources_zonal_stats_table_eligible['Harc_Stream_density_sqkm'],
                                   CHR_sources_zonal_stats_table_eligible['min.BDW'],
                                   CHR_sources_zonal_stats_table_eligible['min.EPX'],
                                   
                                   flow_metrics_50lim_enviro_gams[["GAM : pc75th ~ "]][["pc75th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc84th ~ "]][["pc84th ~ _GAM_predict_CI"]][3:6],
                                   flow_metrics_50lim_enviro_gams[["GAM : pc94th ~ "]][["pc94th ~ _GAM_predict_CI"]][3:6])
  
  
  ## Bind the harc predict together
  
  
  
  
  ## Bind the predictions together
  flow_pred_combo <- bind_rows(flow_pred_10th, 
                               flow_pred_20th,
                               flow_pred_30th,
                               flow_pred_40th,
                               flow_pred_50th) %>% 
    
    # WHEN HR_limit = 10
    mutate(HARC = ifelse(CHR_Water_Source %in% harc_chr_ws, TRUE, FALSE)) %>% 
    
    ## First create the uptake % to use in the final calc
    mutate(Uptake_prop_model = case_when((Harvest_limit == 10) ~ Uptake_prop,
                                         (Harvest_limit == 20 & Uptake_prop <= 1) ~     Uptake_prop,
                                         (Harvest_limit == 20 & Uptake_prop >= 1  &     Uptake_prop <= 2) ~ 1,
                                         (Harvest_limit == 20 & Uptake_prop > 2)  ~ 1 + Uptake_prop - 2,
                                         
                                         (Harvest_limit == 30 & Uptake_prop <= 1) ~     Uptake_prop,
                                         (Harvest_limit == 30 & Uptake_prop >= 1  &     Uptake_prop <= 3) ~ 1,
                                         (Harvest_limit == 30 & Uptake_prop > 3)  ~ 1 + Uptake_prop - 3,
                                         
                                         
                                         (Harvest_limit == 40 & Uptake_prop <= 1) ~ Uptake_prop,
                                         (Harvest_limit == 40 & Uptake_prop >= 1 & Uptake_prop <= 4) ~ 1,
                                         (Harvest_limit == 40 & Uptake_prop > 4) ~ 1 + Uptake_prop - 4,
                                         
                                         (Harvest_limit == 50 & Uptake_prop <= 1) ~ Uptake_prop,
                                         (Harvest_limit == 50 & Uptake_prop >= 1 & Uptake_prop <= 5) ~ 1,
                                         (Harvest_limit == 50 & Uptake_prop > 5) ~ 1 + Uptake_prop - 5,
                                         
                                         TRUE ~ 0)) %>% 
    
    ## Create the final column of Predicted flow.
    ## dQ% =
    ## Area Eligible (%) * dQ Area (modelled) * % uptake
    mutate(Predicted_flow_10 = Eligible_Area * `fit_pc10th ~ ` * Uptake_prop_model,
           Predicted_flow_20 = Eligible_Area * `fit_pc20th ~ ` * Uptake_prop_model,
           Predicted_flow_30 = Eligible_Area * `fit_pc30th ~ ` * Uptake_prop_model,
           Predicted_flow_40 = Eligible_Area * `fit_pc40th ~ ` * Uptake_prop_model,
           Predicted_flow_50 = Eligible_Area * `fit_pc50th ~ ` * Uptake_prop_model,
           
           Predicted_flow_60 = Eligible_Area * `fit_pc60th ~ ` * Uptake_prop_model,
           Predicted_flow_65 = Eligible_Area * `fit_pc65th ~ ` * Uptake_prop_model,
           
           Predicted_flow_70 = Eligible_Area * `fit_pc70th ~ ` * Uptake_prop_model,
           Predicted_flow_75 = Eligible_Area * `fit_pc75th ~ ` * Uptake_prop_model,
           
           Predicted_flow_80 = Eligible_Area * `fit_pc80th ~ ` * Uptake_prop_model,
           Predicted_flow_82 = Eligible_Area * `fit_pc82nd ~ ` * Uptake_prop_model,
           Predicted_flow_84 = Eligible_Area * `fit_pc84th ~ ` * Uptake_prop_model,
           Predicted_flow_86 = Eligible_Area * `fit_pc86th ~ ` * Uptake_prop_model,
           Predicted_flow_88 = Eligible_Area * `fit_pc88th ~ ` * Uptake_prop_model,
           Predicted_flow_90 = Eligible_Area * `fit_pc90th ~ ` * Uptake_prop_model,
           
           Predicted_flow_92 = Eligible_Area * `fit_pc92nd ~ ` * Uptake_prop_model,
           Predicted_flow_94 = Eligible_Area * `fit_pc94th ~ ` * Uptake_prop_model,
           Predicted_flow_96 = Eligible_Area * `fit_pc96th ~ ` * Uptake_prop_model,
           Predicted_flow_98 = Eligible_Area * `fit_pc98th ~ ` * Uptake_prop_model,
           Predicted_flow_100 = Eligible_Area * `fit_pc100th ~ ` * Uptake_prop_model) %>% 
    
    
    select(MAJOR_CATCH, 
           EXTRACTION_MANAGEMENT_UNIT, 
           CHR_Water_Source, 
           HARC,
           Dam_Ha,
           CHR_Ha,
           CHR_Sqkm,
           Harc_Stream_density_sqkm,
           Contour_median,
           Volume_ML,
           Volume_ML_per_Ha,
           Harvest_limit,
           Uptake_prop,
           Uptake_prop_model,
           Eligible_Area,
           min.BDW,
           min.EPX,
           median.Max_temp_warm_month,
           median.Forest_Cov,
           median.Elevation,
           median.Slope,
           
           everything())
  
  flow_pred_CI <- bind_rows(flow_pred_10th_full, 
                            flow_pred_20th_full,
                            flow_pred_30th_full,
                            flow_pred_40th_full,
                            flow_pred_50th_full)
  
  
  
  ## Check the model
  # First, gam.check() reports on model convergence. Here, it reports full convergence. 
  # R has found a best solution. If the model has not converged, results are likely not correct. 
  # This can happen when there are too many parameters in the model for not enough data.
  # 
  # Below, we see a table of basis checking results. This shows a statistical test for patterns 
  # in model residuals, which should be random. Each line reports the test results for one smooth. 
  # It shows the k value or number of basis functions, the effective degrees of freedom, a test 
  # statistic, and p-value.
  # 
  # Here, small p-values indicate that residuals are not randomly distributed. This often means 
  # there are not enough basis functions
  
  
  ## Save model predictions into a workbook ----
  dpe_gam_tables_list <- c('flow_pred_10th',
                           'flow_pred_20th',
                           'flow_pred_30th',
                           'flow_pred_40th',
                           'flow_pred_50th',
                           'flow_pred_combo')
  
  alluv_gam_tables_list <- c('flow_pred_10th_full',
                             'flow_pred_20th_full',
                             'flow_pred_30th_full',
                             'flow_pred_40th_full',
                             'flow_pred_50th_full')
  
  if(exists('HARC_GAM_workbook') == TRUE) {
    message('re-create workbook')
    rm(DPE_HARC_GAM_workbook)
    rm(ALL_HARC_GAM_workbook)
  }
  
  if(write_gam_results) {
    
    DPE_HARC_GAM_workbook <- createWorkbook()
    ALL_HARC_GAM_workbook <- createWorkbook()
    
    for(file in dpe_gam_tables_list) {
      
      ## Get required columns: file <- gam_tables_list[1]
      # File_to_Write <- get(file) %>% as_tibble()
      # 
      # # write_excel_csv(File_to_Write,
      # #                 paste0(data_dir, file, '.csv'))
      
      ## Add worksheet to the spread sheet
      message('writing ', file,  ' to results spreadsheet')
      addWorksheet(DPE_HARC_GAM_workbook, file)
      
      ## Write the data to the corresponding worksheet
      writeDataTable(wb         = DPE_HARC_GAM_workbook,
                     sheet      = file,
                     x          = get(file) %>% as_tibble(),
                     startCol   = 1,
                     startRow   = 1,
                     rowNames   = FALSE,
                     tableStyle = "TableStyleMedium2")
      
    }
    
    saveWorkbook(DPE_HARC_GAM_workbook,
                 paste0(hydro_out, '/harc_models/DPE_HARC_GAM_workbook.xlsx'),
                 overwrite = TRUE)
    
    
    for(file in alluv_gam_tables_list) {
      
      ## Get required columns: file <- gam_tables_list[1]
      # File_to_Write <- get(file) %>% as_tibble()
      # 
      # # write_excel_csv(File_to_Write,
      # #                 paste0(data_dir, file, '.csv'))
      
      ## Add worksheet to the spread sheet
      message('writing ', file,  ' to results csv and spreadsheet')
      addWorksheet(ALL_HARC_GAM_workbook, file)
      
      ## Write the data to the corresponding worksheet
      writeDataTable(wb         = ALL_HARC_GAM_workbook,
                     sheet      = file,
                     x          = get(file) %>% as_tibble(),
                     startCol   = 1,
                     startRow   = 1,
                     rowNames   = FALSE,
                     tableStyle = "TableStyleMedium2")
      
    }
    
    # Save the whole workbook
    saveWorkbook(ALL_HARC_GAM_workbook,
                 paste0(hydro_out, '/harc_models/ALL_HARC_GAM_workbook.xlsx'),
                 overwrite = TRUE)
    
  }
}





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
######################################################################################################################### %>% 