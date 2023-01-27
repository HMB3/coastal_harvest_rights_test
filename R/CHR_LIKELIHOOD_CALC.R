############################################################################################
##############################  ---- CALCULATE LIKELIHOOD ---- #############################
############################################################################################


# \ 
# 
# This code combines the hydro flow data with the 
#
#   \


## Do each criteria separately.
## Then have a separate script for the combination



## STEP 1 :: Prepare Gauge data  ----


## Read in Dushmanta's modelling - important 
baseline_flow_output <- read_excel_allsheets(paste0(hydro_data,
                                                    'Bega_baseline_outputs_WS_Gauges_dummy_data.xlsx'))


## Raw data from Dushmanta's models - 45 rows from 1900 - current
baseline_ws_gauges    <- baseline_flow_output$Bega_baseline_outputs_WS_Gauges
gauge_subset_sort     <- names(baseline_ws_gauges)[2:29]


## Change nams to match - assuming they are in order...
baseline_ws_gauges_20 <- baseline_flow_output$Bega_20_outputs_WS_Ga
baseline_ws_gauges_30 <- baseline_flow_output$Bega_30_outputs_WS_Gauges
baseline_ws_gauges_40 <- baseline_flow_output$Bega_40_outputs_WS_Gauges
baseline_ws_gauges_50 <- baseline_flow_output$Bega_50_outputs_WS_Gauges


## Calculate the % change in flow metrics for each station (column), for each limit (%) 
## First, make one big table with the columns next to each other
names(baseline_ws_gauges_20)[2:29] <- paste0(names(baseline_ws_gauges_20)[2:29], '_20%')
names(baseline_ws_gauges_30)[2:29] <- paste0(names(baseline_ws_gauges_30)[2:29], '_30%')
names(baseline_ws_gauges_40)[2:29] <- paste0(names(baseline_ws_gauges_40)[2:29], '_40%')
names(baseline_ws_gauges_50)[2:29] <- paste0(names(baseline_ws_gauges_50)[2:29], '_50%')


## Master table of the gagues
baseline_ws_gauges_combined <- list(baseline_ws_gauges, 
                                    baseline_ws_gauges_20,
                                    baseline_ws_gauges_30,
                                    baseline_ws_gauges_40,
                                    baseline_ws_gauges_50) %>% 
  
  reduce(left_join, by = "Date") %>% 
  .[ , order(names(.))] %>% dplyr::select("Date", everything())


water_source_gauge_list <- c('baseline_ws_gauges', 
                             'baseline_ws_gauges_20',
                             'baseline_ws_gauges_30',
                             'baseline_ws_gauges_40',
                             'baseline_ws_gauges_50')


# STEP 2 :: Calculate % change in flow ML for each gauge  ----


## % change for each station, each column
## Use the measure that's specific for each water source - that water source has consequence
## All estuary values need the gauges outside the water source - EG DC 1.
## hassle to set it up. Attribute table for each source.
## Likelihood measure of 



## Need to loop through all the gauges, 
## 1). Select the column without % - baseline
## 2). Take mean of each column, then pivot longer
## 3). Create new column for impact model - rows are the % 
## 4). Then calc the % change for all the columns 
CHR_Water_Source_annual_flow_change_RISK <- gauge_subset_sort %>%
  
  ## Pipe the list into lapply
  # loc = gauge_subset_sort[1]
  lapply(function(loc) {
    
    message('calculate % flow change for ', loc)
    
    gauge_names <- baseline_ws_gauges_combined %>% 
      dplyr::select(contains(c(loc))) %>% 
      names()
    
    gague_10 <- gauge_names %>% .[!grepl("%",  .)]
    gague_20 <- gauge_names %>% .[grepl("20%", .)]
    gague_30 <- gauge_names %>% .[grepl("30%", .)]
    gague_40 <- gauge_names %>% .[grepl("40%", .)]
    gague_50 <- gauge_names %>% .[grepl("50%", .)]
    
    gauge_data <- baseline_ws_gauges_combined %>% 
      dplyr::select("Date", contains(c(loc)))
    
    gauge_annual_mean <- gauge_data %>%
      summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
      
      ## Create the uptake factor
      ## % change = mean(ML_future (e.g 20%)) - mean(ML_current (EG 20%))/ML_current * 100
      mutate(Gauge = loc,
             Mn_ann_flw_change_10        =  !!sym(gague_10) - !!sym(gague_10),         
             Mn_ann_flw_perc_change_10   =  Mn_ann_flw_change_10 / !!sym(gague_10) * -100,
             
             Mn_ann_flw_change_20        =  !!sym(gague_10) - !!sym(gague_20),          
             Mn_ann_flw_perc_change_20   =  Mn_ann_flw_change_20 / !!sym(gague_10) * -100,
             
             Mn_ann_flw_change_30        =  !!sym(gague_10) - !!sym(gague_30),         
             Mn_ann_flw_perc_change_30   =  Mn_ann_flw_change_30 / !!sym(gague_10) * -100,
             
             Mn_ann_flw_change_40        =  !!sym(gague_10) - !!sym(gague_40),         
             Mn_ann_flw_perc_change_40   =  Mn_ann_flw_change_40 / !!sym(gague_10) * -100,
             
             Mn_ann_flw_change_50        =  !!sym(gague_10) - !!sym(gague_50),         
             Mn_ann_flw_perc_change_50   =  Mn_ann_flw_change_50 / !!sym(gague_10) * -100)  %>% 
      
      ## Update
      dplyr::select(Gauge, 
                    gague_10,
                    Mn_ann_flw_change_10,
                    Mn_ann_flw_perc_change_10,
                    
                    gague_20,
                    Mn_ann_flw_change_20,
                    Mn_ann_flw_perc_change_20,
                    
                    gague_30,
                    Mn_ann_flw_change_30,
                    Mn_ann_flw_perc_change_30,
                    
                    gague_40,
                    Mn_ann_flw_change_40,
                    Mn_ann_flw_perc_change_40,
                    
                    gague_50,
                    Mn_ann_flw_change_50,
                    Mn_ann_flw_perc_change_50)  %>% 
      
      ## Add columns for the ML values
      rename(Gague_10 = gague_10,
             Gague_20 = gague_20,
             Gague_30 = gague_30,
             Gague_40 = gague_40,
             Gague_50 = gague_50) %>% 
      
      ## Create the risk score
      mutate(Likelihood_20 = case_when(Mn_ann_flw_perc_change_20 < 20 ~ 1,
                                       Mn_ann_flw_perc_change_20 >= 20  & Mn_ann_flw_perc_change_20 < 50 ~ 2,
                                       Mn_ann_flw_perc_change_20 >= 50 ~ 3,
                                       TRUE ~ as.numeric(Mn_ann_flw_perc_change_20)),
             
             Likelihood_30 = case_when(Mn_ann_flw_perc_change_30 < 20 ~ 1,
                                       Mn_ann_flw_perc_change_30 >= 20  & Mn_ann_flw_perc_change_30 < 50 ~ 2,
                                       Mn_ann_flw_perc_change_30 >= 50 ~ 3,
                                       TRUE ~ as.numeric(Mn_ann_flw_perc_change_30)),
             
             Likelihood_40 = case_when(Mn_ann_flw_perc_change_40  < 20 ~ 1,
                                       Mn_ann_flw_perc_change_40 >= 20  & Mn_ann_flw_perc_change_40 < 50 ~ 2,
                                       Mn_ann_flw_perc_change_40 >= 50 ~ 3,
                                       TRUE ~ as.numeric(Mn_ann_flw_perc_change_40)),
             
             Likelihood_50 = case_when(Mn_ann_flw_perc_change_50  < 20 ~ 1,
                                       Mn_ann_flw_perc_change_50 >= 20  & Mn_ann_flw_perc_change_50 < 50 ~ 2,
                                       Mn_ann_flw_perc_change_50 >= 50 ~ 3,
                                       TRUE ~ as.numeric(Mn_ann_flw_perc_change_50)))
    
  }) %>% bind_rows(.)



## Save change data
write_csv(CHR_Water_Source_annual_flow_change_RISK,
          paste0(likely_out, 'CHR_Water_Source_annual_flow_change_RISK.csv'))





# STEP 3 :: Calculate flow metrics across the time series for each limit  ----


## First, calculate the flow metrics for the baseline.
## then calculate the % change

if(flow_metrics) {
  
  
  ## test flow duration
  test_flow_10_per <- fdc(baseline_ws_gauges_combined$`BEGA_219001_Rutherford Brown Mtn`,
                          lQ.thr = 0.9,
                          hQ.thr = 0.1,
                          main = "Water Source 1 :: Flow Duration Curve")
  
  
  test_flow_10_per <- fdc.data.frame(baseline_ws_gauges_combined$`BEGA_219001_Rutherford Brown Mtn`,
                                     lQ.thr = 0.9,
                                     hQ.thr = 0.1,
                                     main = "Water Source 1 :: Flow Duration Curve")
  
  hydro_results_gauges_100_50_table <-
    
    hydrostats_table(gauge_list      = gauge_list,  ## list of tables
                     model_run        = '_WithDams_100_50_2',
                     column           = 'Q_WithDams_100_50_2',
                     compare_colum    = 'Q_NoDams_100_50_2',
                     
                     length_threshold = 1000, ## Flow threshold, change this
                     high_quart       = 0.9,
                     low_quart        = 0.1)
  
  
  hydro_results_gauges_100_30_table  <- 
    
    hydrostats_table(gauge_list       = harc_list,
                     model_run        = '_WithDams_100_30_2',
                     column           = 'Q_WithDams_100_30_2',
                     compare_colum    = 'Q_NoDams_100_30_2',
                     
                     length_threshold = 1000, ## Flow threshold, change this
                     high_quart       = 0.9,
                     low_quart        = 0.1)
  
  
  hydro_results_gauges_100_20_table <- 
    
    hydrostats_table(gauge_list       = harc_list,
                     model_run        = '_WithDams_100_20_2',
                     column           = 'Q_WithDams_100_20_2',
                     compare_colum    = 'Q_NoDams_100_20_2',
                     
                     length_threshold = 1000, ## Flow threshold, change this
                     high_quart       = 0.9,
                     low_quart        = 0.1)
  
  
  hydro_results_gauges_100_10_table <- 
    
    hydrostats_table(gauge_list      = harc_list,
                     model_run        = '_WithDams_100_10_2',
                     column           = 'Q_WithDams_100_10_2',
                     compare_colum    = 'Q_NoDams_100_10_2',
                     
                     length_threshold = 1000, ## Flow threshold, change this
                     high_quart       = 0.9,
                     low_quart        = 0.1)
  
  
  ## Bind the tables together
  hydro_results_gauges_all <- bind_rows(hydro_results_gauges_100_10_2_table,
                                        hydro_results_gauges_100_20_2_table,
                                        hydro_results_gauges_100_30_2_table,
                                        hydro_results_gauges_100_50_2_table) %>% 
    
    ## Create the uptake factor
    mutate(Uptake_percent = case_when(grepl("Q_WithDams_100", Model) ~ 100),
           
           ## Create the limit factor
           Limit_percent = case_when(grepl("10_2", Model) ~ 10,
                                     grepl("20_2", Model) ~ 20,
                                     grepl("30_2", Model) ~ 30,
                                     grepl("50_2", Model) ~ 50))
  
}



## Step 4 :: INTEGRATE HYDROLOGY DATA SOURCES ----


## Bring in farm dam data
NSW_CHR_Water_Sources_Farm_dams <- read_csv(paste0(data_dir, 'NSW_CHR_Water_Sources_Farm_dams.csv'))







################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################