############################################################################################
##############################  ---- CALCULATE LIKELIHOOD ---- #############################
############################################################################################


# \ 
# 
# This code calculates likelihood for the CHR data
#
#   \


## Do each criteria separately.
## Then have a separate script for the combination



## STEP 1 :: Prepare Gauge data  ----


if(likely) {
  
  
  message('Calculating Likliehood Risk per Measure/DC/Theme for each Water Source')
  
  
  ## Read in CTP thresh
  CTP_data <- read_excel_allsheets(paste0(hydro_data,
                                          '/Cease_to_pump_rules_for_coastal_water_sources_CHR_input.xlsx'))
  
  
  CTP_thresholds <- CTP_data$`Input for tool` %>% 
    
    select('Water Source Name_CHR',
           "Station #",
           "Threshold (ML/day)") %>% 
    
    rename(CHR_Water_Source = 'Water Source Name_CHR',
           Gauge_station    = "Station #",
           Threshold_ML     = "Threshold (ML/day)")  %>% 
    na.omit() %>% 
    mutate(Gauge_station = gsub('N/A', NA, Gauge_station))
  
  
  
  
  ## Raw data from Dushmanta's models - 45 rows from 1900 - current
  # baseline_ws_gauges    <- baseline_flow_output$Bega_baseline_outputs_WS_Gauges
  # gauge_subset_sort     <- names(baseline_ws_gauges)[2:29]
  
  
  baseline_ws_gauges       <- read_csv(paste0(hydro_data, 'Bega/', 'Bega_baseline_outputs_WS_Gauges.csv')) %>% 
    mutate(Seq_date = seq(ymd('1889-01-01'),ymd('2019-07-31'), by = '1 day')) %>% 
    select(-Date) %>% 
    rename(Date = Seq_date) %>% select(Date, everything())
  
  
  baseline_ws_gauges_10    <- read_csv(paste0(hydro_data, 'Bega/', 'Bega_HL10_outputs_WS_Gauges.csv')) %>% 
    mutate(Seq_date = seq(ymd('1889-01-01'),ymd('2019-07-31'), by = '1 day')) %>% 
    select(-Date) %>% 
    rename(Date = Seq_date) %>% select(Date, everything())
  
  
  baseline_ws_gauges_20    <- read_csv(paste0(hydro_data, 'Bega/', 'Bega_HL20_outputs_WS_Gauges.csv')) %>% 
    mutate(Seq_date = seq(ymd('1889-01-01'),ymd('2019-07-31'), by = '1 day')) %>% 
    select(-Date) %>% 
    rename(Date = Seq_date) %>% select(Date, everything())
  
  
  baseline_ws_gauges_30    <- read_csv(paste0(hydro_data, 'Bega/', 'Bega_HL30_outputs_WS_Gauges.csv')) %>% 
    mutate(Seq_date = seq(ymd('1889-01-01'),ymd('2019-07-31'), by = '1 day')) %>% 
    select(-Date) %>% 
    rename(Date = Seq_date) %>% select(Date, everything())
  
  
  baseline_ws_gauges_40    <- read_csv(paste0(hydro_data, 'Bega/', 'Bega_HL40_outputs_WS_Gauges.csv')) %>% 
    mutate(Seq_date = seq(ymd('1889-01-01'),ymd('2019-07-31'), by = '1 day')) %>% 
    select(-Date) %>% 
    rename(Date = Seq_date) %>% select(Date, everything())
  
  
  baseline_ws_gauges_50    <- read_csv(paste0(hydro_data, 'Bega/', 'Bega_HL50_outputs_WS_Gauges.csv')) %>% 
    mutate(Seq_date = seq(ymd('1889-01-01'),ymd('2019-07-31'), by = '1 day')) %>% 
    select(-Date) %>% 
    rename(Date = Seq_date) %>% select(Date, everything())
  
  
  ## Rename for clarity 
  names(baseline_ws_gauges) <- str_replace_all(names(baseline_ws_gauges), "[[:punct:]]", " ") %>% 
    gsub('   ',  ' ', .) %>% 
    gsub('  ',  ' ',  .) %>% 
    gsub(' ',  ' ',   .) %>%
    gsub(' ',  '_',   .)
  
  # names(baseline_ws_gauges_10) <- names(baseline_ws_gauges) 
  # names(baseline_ws_gauges_20) <- names(baseline_ws_gauges)
  # names(baseline_ws_gauges_30) <- names(baseline_ws_gauges)
  
  
  ## Calculate the % change in flow metrics for each station (column), for each limit (%) 
  ## First, make one big table with the columns next to each other
  names(baseline_ws_gauges_10)[2:29] <- paste0(names(baseline_ws_gauges)[2:29], '_10%')
  names(baseline_ws_gauges_20)[2:29] <- paste0(names(baseline_ws_gauges)[2:29], '_20%')
  names(baseline_ws_gauges_30)[2:29] <- paste0(names(baseline_ws_gauges)[2:29], '_30%')
  names(baseline_ws_gauges_40)[2:29] <- paste0(names(baseline_ws_gauges)[2:29], '_40%')
  names(baseline_ws_gauges_50)[2:29] <- paste0(names(baseline_ws_gauges)[2:29], '_50%')
  
  
  ## Master table of the gauges
  bega_baseline_ws_gauge_flows_combined <- list(baseline_ws_gauges, 
                                                baseline_ws_gauges_10,
                                                baseline_ws_gauges_20,
                                                baseline_ws_gauges_30, 
                                                baseline_ws_gauges_40,
                                                baseline_ws_gauges_50) %>% 
    
    reduce(left_join, by = "Date") %>% 
    .[ , order(names(.))] %>% dplyr::select("Date", everything())
  
  
  
  ## Save the full table of hydro data
  write_csv(baseline_ws_gauges_combined, paste0(hydro_data, 'bega_baseline_ws_gauge_flows_combined.csv'))
  
  
  ## We want to run hydro_stats on every column
  all_column_list <- names(bega_baseline_ws_gauge_flows_combined) %>% .[-1]
  
  
  
  gauge_baseline_cols <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[!grepl('%', .)] %>% .[-1]  %>% 
    .[!grepl('Water_Source', .)]
  
  
  ws_baseline_cols <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[!grepl('%', .)] %>% .[-1]  %>% 
    .[grepl('Water_Source', .)]
  
  
  gauge_percent_cols <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[grepl('%', .)] %>% .[-1] %>% 
    .[grepl('Water_Source', .)]
  
  
  ws_percent_cols <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[grepl('%', .)] %>% .[-1] %>% 
    .[grepl('Water_Source', .)]
  
  
  ## We want to run hydro_stats on every column
  HR10_column_list <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[grepl('10%', .)] %>% .[-1]
  
  HR20_column_list <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[grepl('20%', .)] %>% .[-1]
  
  HR30_column_list <- names(bega_baseline_ws_gauge_flows_combined) %>% 
    
    .[grepl('30%', .)] %>% .[-1]
  
  
  
  # STEP 2 :: Calculate flow metrics across the time series for each limit  ----
  
  
  ## First, calculate the flow metrics for the baseline.
  ## then calculate the % change
  
  if(flow_metrics) {
    
    
    ## First, do hydrostats.
    ## Need the 5th and 95th percentile
    
    ## test flow duration
    # message('creating hydrography for ', col)
    # run hydro_graphs for each set - they need 10/20/30 for each station
    # baseline_flow <- fdc(baseline_ws_gauges_combined$BEGA_219001_Rutherford_Brown_Mtn,
    #                             lQ.thr = 0.9,
    #                             hQ.thr = 0.1,
    #                             main   = "Water Source 1 :: Flow Duration Curve")
    
    flow_years <- (nrow(bega_baseline_ws_gauge_flows_combined)/365) %>% round(., 0)
    
    
    hydro_metrics_bega_all_gauges_baseline <-
      
      hydrostats_station_metrics(col_list         = baseline_column_list,  ## list of tables
                                 hydro_table      = bega_baseline_ws_gauge_flows_combined,
                                 CTP_table        = CTP_thresholds,
                                 years            = flow_years,
                                 length_threshold = 1000, ## Flow threshold, change this
                                 high_quart       = 0.95,
                                 low_quart        = 0.05) %>% 
      
      ## Make a column for limit
      mutate(Limit_percent = case_when(!grepl('%', Water_gauge) ~ 0)) %>% 
      
      mutate(Gauge_name = gsub('Water_Source_', '', Water_gauge),
             Gauge_name = gsub('_', ' ', Gauge_name)) %>%
      
      regex_left_join(., NSW_CHR_WS_EMU_LUT, 
                      by = c('Gauge_name' = 'CHR_Water_Source')) %>% 
      
      dplyr::select(CHR_Water_Source, 
                    EXTRACTION_MANAGEMENT_UNIT,
                    Gauge_name, 
                    Gauge_num, 
                    -Water_gauge,
                    Limit_percent, 
                    CTP_thresh, 
                    CTP_days, everything())
    
    
    ## Save the full table of hydro metrics for each station
    write_csv(hydro_metrics_bega_all_gauges_baseline, 
              paste0(hydro_out, 'BEGA_hydro_metrics_all_gauge_baseline.csv'))
    
    
    # rem_dup.one <- function(x){
    #   paste(unique(tolower(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T))))),collapse = " ")
    # }
    # 
    # rem_dup.vector <- Vectorize(rem_dup.one,USE.NAMES = F)
    # rem_dup.vector('Water Source Bega River Estuary and Tributaries Water Source 10%')
    
    
    
    ## 
    BEGA_hydro_metrics_gauge_percents <-
      
      hydrostats_station_metrics(col_list         = gauge_percent_cols[1:6],  ## list of tables
                                 hydro_table      = bega_baseline_ws_gauge_flows_combined,
                                 CTP_table        = CTP_thresholds,
                                 years            = flow_years,
                                 length_threshold = 1000, ## Flow threshold, change this
                                 high_quart       = 0.95,
                                 low_quart        = 0.05) %>% 
      
      ## Make a column for limit
      mutate(Limit_percent = case_when(grepl('10%', Water_gauge) ~ 10,
                                       grepl('20%', Water_gauge) ~ 20,
                                       grepl('30%', Water_gauge) ~ 30,
                                       grepl('40%', Water_gauge) ~ 40,
                                       grepl('50%', Water_gauge) ~ 50,
                                       !grepl('%',  Water_gauge) ~ 0)) %>% 
      
      mutate(#Gauge_name = gsub('Water_Source_', '', Water_gauge),
             Gauge_name = gsub('_', ' ',    Water_gauge),
             Gauge_name = paste(unique(Gauge_name), collapse = ' ')) %>%
      
      stringdist_left_join(., NSW_CHR_WS_EMU_LUT, 
                           by = c('Gauge_name' = 'CHR_Water_Source'), distance = 3) %>% 
      
      dplyr::select(CHR_Water_Source, 
                    EXTRACTION_MANAGEMENT_UNIT,
                    Gauge_name, 
                    Gauge_num, 
                    -Water_gauge,
                    Limit_percent, 
                    CTP_thresh, 
                    CTP_days, everything())
    
    
    ## Save the full table of hydro metrics for each station
    write_csv(BEGA_hydro_metrics_all_gauge_limits, 
              paste0(hydro_out, 'BEGA_hydro_metrics_all_gauge_limits.csv'))
    
    
  }
  
  
  # STEP 3 :: Calculate % change in flow ML for each gauge and WS ----
  
  
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
      
      gauge_10 <- gauge_names %>% .[!grepl("%",  .)]
      gauge_20 <- gauge_names %>% .[grepl("20%", .)]
      gauge_30 <- gauge_names %>% .[grepl("30%", .)]
      gauge_40 <- gauge_names %>% .[grepl("40%", .)]
      gauge_50 <- gauge_names %>% .[grepl("50%", .)]
      
      gauge_data <- baseline_ws_gauges_combined %>% 
        dplyr::select("Date", contains(c(loc)))
      
      gauge_annual_mean <- gauge_data %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
        
        ## Create the uptake factor
        ## % change = mean(ML_future (e.g 20%)) - mean(ML_current (EG 20%))/ML_current * 100
        mutate(Gauge = loc,
               Mn_ann_flw_change_10        =  !!sym(gauge_10) - !!sym(gauge_10),         
               Mn_ann_flw_perc_change_10   =  Mn_ann_flw_change_10 / !!sym(gauge_10) * -100,
               
               Mn_ann_flw_change_20        =  !!sym(gauge_10) - !!sym(gauge_20),          
               Mn_ann_flw_perc_change_20   =  Mn_ann_flw_change_20 / !!sym(gauge_10) * -100,
               
               Mn_ann_flw_change_30        =  !!sym(gauge_10) - !!sym(gauge_30),         
               Mn_ann_flw_perc_change_30   =  Mn_ann_flw_change_30 / !!sym(gauge_10) * -100,
               
               Mn_ann_flw_change_40        =  !!sym(gauge_10) - !!sym(gauge_40),         
               Mn_ann_flw_perc_change_40   =  Mn_ann_flw_change_40 / !!sym(gauge_10) * -100,
               
               Mn_ann_flw_change_50        =  !!sym(gauge_10) - !!sym(gauge_50),         
               Mn_ann_flw_perc_change_50   =  Mn_ann_flw_change_50 / !!sym(gauge_10) * -100)  %>% 
        
        ## Update
        dplyr::select(Gauge, 
                      gauge_10,
                      Mn_ann_flw_change_10,
                      Mn_ann_flw_perc_change_10,
                      
                      gauge_20,
                      Mn_ann_flw_change_20,
                      Mn_ann_flw_perc_change_20,
                      
                      gauge_30,
                      Mn_ann_flw_change_30,
                      Mn_ann_flw_perc_change_30,
                      
                      gauge_40,
                      Mn_ann_flw_change_40,
                      Mn_ann_flw_perc_change_40,
                      
                      gauge_50,
                      Mn_ann_flw_change_50,
                      Mn_ann_flw_perc_change_50)  %>% 
        
        ## Add columns for the ML values
        rename(gauge_10 = gauge_10,
               gauge_20 = gauge_20,
               gauge_30 = gauge_30,
               gauge_40 = gauge_40,
               gauge_50 = gauge_50) %>% 
        
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
  
  
  
  ## Step 4 :: INTEGRATE HYDROLOGY DATA SOURCES ----
  
  
  ## Petter will put the Rules for the source combination in the LUT
  
  ## Bring in farm dam data
  # NSW_CHR_Water_Sources_Farm_dams <- read_csv(paste0(daa_dir, 'NSW_CHR_Water_Sources_Farm_dams.csv'))
  
  
  
}


message('CHR Likliehood per Measure/DC/Theme has been pre-calculated for each Water Source')



################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################