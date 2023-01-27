#########################################################################################################################
###############################  ------ HARC MODEL ANALYSIS ---- ########################################################
#########################################################################################################################


# \ 
# 
# This code analyzes the HARC models at different extraction levels
#   
#   this is an annotation test
#   \


## ENVIRONMENT SETTINGS =============================================================


## To-do

## Check hydro stats parametization
## Check flow metrics


# STEP 1 :: Get HARC Data ----

if(harc_models) {
  
  
  ## Each file is a water source, with a code for the area.
  ## What are those codes?
  HARC_data   <- read_excel_allsheets('./data/enviro/hydro/HARC/HARC_10xFlowSequences.xlsx')
  
  
  names(HARC_data)
  
  
  
  ## How are the data formatted?
  
  ## Cathchment names 
  # Duc   = Duck Creek
  # Woo   = Woolgoolga Creek
  # Buc   = Bucca Bucca River
  # Nam   = Nambucca River
  # Allyn = Allyn River
  # Wyo   = Wyong River
  # Wolbi = Wollombi Brook
  # Woldy = Wollondilly River
  # Dou   = Double Creek
  # Bem   = Bemboka River
  
  
  ## Try looping this?
  years <- 2016-1975
  
  
  # mutate(CHR_Water_Source = gsub('Bemboka Rivers', 
  #                                'Upper Bega/Bemboka Rivers Tributaries', CHR_Water_Source),
  #        
  #        CHR_Water_Source = gsub('Wollombi Brook', 
  #                                'Upper Wollombi Brook', CHR_Water_Source),
  #        
  #        CHR_Water_Source = gsub('Nambucca River', 
  #                                'Coastal Nambucca River', CHR_Water_Source),
  
  
  Allyn_2nd <- HARC_data$AllDataAllyn2nd %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source = 'Paterson/Allyn Rivers Water Source',   
                                                    .before = Date)
  
  Bem_2nd   <- HARC_data$AllDataBem2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source = 'Upper Bega/Bemboka Rivers Tributaries Water Source', .before = Date)
  
  Buc_2nd   <- HARC_data$AllDataBuc2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source = 'Bucca Bucca Creek Water Source', .before = Date)
  
  Dou_2nd   <- HARC_data$AllDataDou2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source = 'Double Crossing Creek Water Source',  .before = Date)
  
  Duc_2nd   <- HARC_data$AllDataDuc2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source = 'Duck Creek Water Source',    .before = Date)
  
  Nam_2nd   <- HARC_data$AllDataNam2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source  = 'Coastal Nambucca River Water Source', .before = Date)
  
  Woo_2nd   <- HARC_data$AllDataWoo2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source  = 'Woolgoolga Creek Water Source', .before = Date)
  
  Woldy_2nd <- HARC_data$AllDataWoldy2nd %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source  = 'Wollondibby Creek Water Source', .before = Date)
  
  Wolbi_2nd <- HARC_data$AllDataWolbi2nd %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source  = 'Upper Wollombi Brook Water Source',    .before = Date)
  
  Wyo_2nd   <- HARC_data$AllDataWyo2nd   %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
                                                    CHR_Water_Source  = 'Wyong River Water Source', .before = Date)
  
  
  ## what are the columns :
  names(Duc_2nd)
  harc_list <- c('Allyn_2nd', 'Bem_2nd', 'Buc_2nd',   'Dou_2nd',   'Duc_2nd',
                 'Nam_2nd',   'Woo_2nd', 'Woldy_2nd', 'Wolbi_2nd', 'Wyo_2nd')
  
  
  
  
  
  # STEP 2 :: Aggregate HARC flow data ----
  
  
  # I’ve attached all the flow sequences in one spreadsheet.  I used these for developing stats for some fact sheets.   
  # I believe every flow sequences derived by HARC for the 10 Water Sources are attached in the spreadsheet.   
  # Every sequence is 42 years in length (1975 to 2016).  There are 2 sets of sequences for each of the Water Sources; 
  # (a) one where dams are only permitted on 1st and 2nd order stream, and 
  # (b) another where they are also permitted on 3rd order streams.  
  
  
  
  # The naming convention for each model run (column heading) is as below (note, 3 sequences for each model run):
  #   
  # Q_impact_28_10_2  
  # “Q” = discharge, 
  # “impact” = difference between with dams & no dams,  
  # “28” = percentage uptake of Harvest Rights (HR), 
  # “10” = HR of 10% of regional runoff, 
  # “2” = dams permitted on 1st and 2nd order streams
  # 
  # Q_NoDams_28_10_2     
  # “Q” = discharge, 
  # “NoDams” = scenario simulates no dams,                        
  # “28” = percentage uptake of HR, 
  # “10” = HR of 10% of regional runoff, 
  # “2” = dams permitted on 1st and 2nd order streams
  # 
  # Q_WithDams_28_10_2 
  # “Q” = discharge, 
  # “WithDams” = scenario simulates dams,                           
  # “28” = percentage uptake of HR, 
  # “10” = HR of 10% of regional runoff, 
  # “2” = dams permitted on 1st and 2nd order streams
  
  # Note, the percentage uptake of HR go 28, 50, 75, and 100, but the estimate of current uptake is also shown 
  # (e.g. 13 below but is different for each of the 10 Water Sources)
  # Note, the HR of 10%, 20%, 30%, and 50% of regional runoff (e.g. 30 below).
  
  
  # Note, “2” indicates dams permitted on 1st and 2nd order streams, while “3” indicates dams permitted on 
  # 3rd order streams as well (e.g. 3 below)
  
  
  # Q_impact_13_30_3        
  # “Q” = discharge, 
  # “impact” = difference between with dams & no dams,       
  # “13” = percentage uptake of HR 
  # “30” = HR of 10% of regional runoff 
  # “3” = dams permitted on 1st 2nd and 3rd order streams
  
  
  ## So we only want the 2nd order, and only the 'impact'?
  ## Calc Mean Annual Flow Change (numeric)
  
  
  ## Tabulate impact data for 100% uptake ----
  harc_annual_impact_100_df <- harc_list %>%
    
    ## Pipe the list into lapply
    #source = harc_list[1]
    lapply(function(source) {
      
      message('calculate % flow change for ', source)
      impact_data = get(source)
      
      impact_models_100 <- impact_data %>% 
        dplyr::select(CHR_Water_Source , contains(c('impact_100'))) %>% 
        names() %>% .[-1]
      
      nodams_models_100 <- impact_data %>% 
        dplyr::select(CHR_Water_Source , contains(c('NoDams_100'))) %>% 
        names() %>% .[-1]
      
      dams_models_100 <- impact_data %>% 
        dplyr::select(CHR_Water_Source , contains(c('WithDams_100'))) %>% 
        names() %>% .[-1]
      
      Change_annual_no_dams <- impact_data %>%
        
        group_by(CHR_Water_Source ) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        dplyr::select(-'Day of Year', -'Month of Year', -'Season Index')   %>%
        dplyr::select(CHR_Water_Source ,contains(c('Q_NoDams_100'))) %>%
        
        ## Column names are the rows, values are the MAF
        ## Calc Mean Annual Flow Change (numeric)
        pivot_longer(., cols = all_of(nodams_models_100),
                     values_to = 'NoDam_mn_ann_flw',
                     names_to  = "NoDam_Model")
      
      Change_annual_dams <- impact_data %>%
        
        group_by(CHR_Water_Source ) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        dplyr::select(-'Day of Year', -'Month of Year', -'Season Index')   %>%
        dplyr::select(CHR_Water_Source ,contains(c('Q_WithDams_100'))) %>%
        
        ## Column names are the rows, values are the MAF
        ## Calc Mean Annual Flow Change (numeric)
        pivot_longer(., cols = all_of(dams_models_100),
                     values_to = 'Dam_mn_ann_flw',
                     names_to  = "Dam_Model")
      
      Change_annual_impact <- impact_data %>%
        
        group_by(CHR_Water_Source ) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        dplyr::select(-'Day of Year', -'Month of Year', -'Season Index')   %>%
        dplyr::select(CHR_Water_Source , contains(c('impact_100'))) %>%
        
        ## Column names are the rows, values are the MAF
        ## Calc Mean Annual Flow Change (numeric)
        pivot_longer(., cols = all_of(impact_models_100),
                     values_to = 'Impact_mn_ann_flw',
                     names_to  = "Impact_Model") %>% 
        
        bind_cols(., Change_annual_no_dams[c("NoDam_Model",          
                                             "NoDam_mn_ann_flw")]) %>% 
        
        bind_cols(., Change_annual_dams[c("Dam_Model",          
                                          "Dam_mn_ann_flw")])      %>% 
        
        ## Create the uptake factor
        mutate(Uptake_percent = case_when(grepl("Q_impact_100_", Impact_Model) ~ 100),
               
               ## Create the limit factor
               Limit_percent = case_when(grepl("10_2", Impact_Model) ~ 10,
                                         grepl("20_2", Impact_Model) ~ 20,
                                         grepl("30_2", Impact_Model) ~ 30,
                                         grepl("50_2", Impact_Model) ~ 50),
               
               Mn_ann_flw_perc_change   = Impact_mn_ann_flw / NoDam_mn_ann_flw *-100,
               Mn_basel_flw_perc_change = (Dam_mn_ann_flw - NoDam_mn_ann_flw)/NoDam_mn_ann_flw *100)  %>% 
        
        dplyr::select(CHR_Water_Source, 
                      NoDam_Model,
                      Dam_Model,
                      Impact_Model, 
                      NoDam_mn_ann_flw, 
                      Dam_mn_ann_flw,
                      Impact_mn_ann_flw,
                      Uptake_percent,
                      Limit_percent,
                      Mn_basel_flw_perc_change,
                      Mn_ann_flw_perc_change) 
      
    }) %>% bind_rows()
  
  
  
  
  
  # STEP 3 :: Calculate flow metrics across the time series  ----
  
  
  
  ## Create tables for all CHR_Water_Source s at 100% uptake, but different limit %
  ## Also, calculate the maximum 
  hydro_results_Dams_100_50_2_table <-
    
    hydrostats_dam_table(source_list      = harc_list,
                         model_run        = '_WithDams_100_50_2',
                         source_col       = 'CHR_Water_Source',
                         column           = 'Q_WithDams_100_50_2',
                         compare_colum    = 'Q_NoDams_100_50_2',
                         
                         length_threshold = 1000, ## Flow threshold, change this
                         high_quart       = 0.9,
                         low_quart        = 0.1)
  
  
  hydro_results_Dams_100_30_2_table  <- 
    
    hydrostats_dam_table(source_list       = harc_list,
                         model_run        = '_WithDams_100_30_2',
                         source_col       = 'CHR_Water_Source',
                         column           = 'Q_WithDams_100_30_2',
                         compare_colum    = 'Q_NoDams_100_30_2',
                         
                         length_threshold = 1000, ## Flow threshold, change this
                         high_quart       = 0.9,
                         low_quart        = 0.1)
  
  
  hydro_results_Dams_100_20_2_table <- 
    
    hydrostats_dam_table(source_list       = harc_list,
                         model_run        = '_WithDams_100_20_2',
                         source_col       = 'CHR_Water_Source',
                         column           = 'Q_WithDams_100_20_2',
                         compare_colum    = 'Q_NoDams_100_20_2',
                         
                         length_threshold = 1000, ## Flow threshold, change this
                         high_quart       = 0.9,
                         low_quart        = 0.1)
  
  
  hydro_results_Dams_100_10_2_table <- 
    
    hydrostats_dam_table(source_list      = harc_list,
                         model_run        = '_WithDams_100_10_2',
                         source_col       = 'CHR_Water_Source',
                         column           = 'Q_WithDams_100_10_2',
                         compare_colum    = 'Q_NoDams_100_10_2',
                         
                         length_threshold = 1000, ## Flow threshold, change this
                         high_quart       = 0.9,
                         low_quart        = 0.1)
  
  
  ## Bind the tables together
  hydro_results_Dams_all <- bind_rows(hydro_results_Dams_100_10_2_table,
                                      hydro_results_Dams_100_20_2_table,
                                      hydro_results_Dams_100_30_2_table,
                                      hydro_results_Dams_100_50_2_table) %>% 
    
    ## Create the uptake factor
    mutate(Uptake_percent = case_when(grepl("Q_WithDams_100", Dam_Model) ~ 100),
           
           ## Create the limit factor
           Limit_percent = case_when(grepl("10_2", Dam_Model) ~ 10,
                                     grepl("20_2", Dam_Model) ~ 20,
                                     grepl("30_2", Dam_Model) ~ 30,
                                     grepl("50_2", Dam_Model) ~ 50))
  
  
  
  ## Need to combine these with the mean annual flow change table created above
  harc_annual_impact_100_Dams_flow_metrics_df <- harc_annual_impact_100_df %>% 
    
    left_join(., hydro_results_Dams_all,
              
              by = c("CHR_Water_Source", "Dam_Model", "Uptake_percent", "Limit_percent")) %>% 
    
    ## Rename these later
    ## (,,Mean_dry_spell_dam - Mean_dry_spell_no_dam) / Mean_dry_spell_no_dam * 100
    mutate(baseflow_perc_change   = (mean.bf.x     - mean.bf.y)     / mean.bf.y     * 100,
           high_spell_perc_change = (high_spell.x  - high_spell.y)  / high_spell.y  * 100,
           low_spell_perc_change  = (low_spell.x   - low_spell.y)   / low_spell.y   * 100,
           seasonal_perc_change   = (seasonality.x - seasonality.y) / seasonality.y * 100) %>% 
    
    ## For the flow metrics, also need the % change : 
    ## reverse order (Dam_mn_ann_flw - NoDam_mn_ann_flw)/NoDam_mn_ann_flw
    dplyr::select(CHR_Water_Source,
                  NoDam_Model,
                  Impact_Model,
                  Dam_Model,
                  NoDam_mn_ann_flw,
                  Dam_mn_ann_flw,
                  Impact_mn_ann_flw,
                  
                  Uptake_percent,
                  Limit_percent,
                  Mn_basel_flw_perc_change,
                  Mn_ann_flw_perc_change,
                  baseflow_perc_change,
                  high_spell_perc_change,
                  low_spell_perc_change,
                  seasonal_perc_change)   %>% 
    
    as.data.frame()                       %>% 
    mutate_all(~ifelse(is.nan(.), NA, .)) %>% 
    mutate_all(~ifelse(is.na(.),   0, .))
  
  
  ## Create a zero df :
  zero_limits <- harc_annual_impact_100_Dams_flow_metrics_df %>% 
    
    filter(NoDam_Model == 'Q_NoDams_100_10_2')  
  
  zero_lims <- data.frame(lapply(zero_limits, function(x) {
    gsub("100_10", "100_0", x)})) %>% 
    
    mutate(Limit_percent    = 0,
           Uptake_percent   = as.numeric(Uptake_percent),
           NoDam_mn_ann_flw = 0,
           Dam_mn_ann_flw   = 0,
           Impact_mn_ann_flw        = 0,
           Mn_basel_flw_perc_change = 0,
           Mn_ann_flw_perc_change   = 0,
           baseflow_perc_change     = 0,
           high_spell_perc_change   = 0,
           low_spell_perc_change    = 0,
           seasonal_perc_change     = 0)
  
  
  
  ## Full df that goes to zero
  harc_annual_impact_100_Dams_flow_metrics_zero_df <- 
    
    harc_annual_impact_100_Dams_flow_metrics_df %>% 
    bind_rows(., zero_lims)                     %>% 
    arrange(CHR_Water_Source , NoDam_Model)     %>% 
    mutate(HARC = "HARC")  
  
  
  # STEP 4 :: Plot metrics at each limit ----
  
  # A.	Limit % vs. Mean annual flow change% @ current uptake
  # Single factor analysis to see if CHR_Water_Source  has an effect mean annual flow change with changes in limit. 
  # Will be interesting to see if the slope is same. From what I can see from the reports relationships within 
  # CHR_Water_Source  will be linear but the slope between them (and the starting point) will be interesting. Will need 
  # to have the points plotted as well.
  
  
  ## For each HARC CHR_Water_Source , create a line plot of each metric
  ## baseflow change and mean annual change should be different
  harc_Mean_ann_flw_perc_changes <- 
    
    harc_annual_impact_100_Dams_flow_metrics_zero_df %>% 
    
    ggplot(aes(Limit_percent, 
               Mn_ann_flw_perc_change, 
               color = CHR_Water_Source )) +
    
    geom_line(size = 2, alpha = .8) +
    theme_light(base_size = 16) +
    
    ##
    labs(title = paste("Limit % vs. Mean annual flow change % @ Full uptake " , 
                       "\n", sep = ""),
         x = 'Limit (%)',
         y = "Mean annual Flow change (%)\n\n") +  
    
    ## Create the themes
    theme_light(base_size = 16) +
    
    theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
          axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size   = 28),
          axis.title.y = element_text(face   = "bold", 
                                      colour = "black", 
                                      size   = 28),
          legend.position  = 'none',
          legend.title     = element_blank(),
          
          panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey")) +
    
    expand_limits(x = 0, y = 0)
  
  
  
  
  harc_Mean_baseline_perc_changes <- 
    
    harc_annual_impact_100_Dams_flow_metrics_zero_df %>% 
    
    ggplot(aes(Limit_percent, 
               Mn_basel_flw_perc_change, 
               color = CHR_Water_Source )) +
    
    geom_line(size = 2, alpha = .8) +
    theme_light(base_size = 16) +
    
    ##
    labs(title = paste("Limit % vs. Mean baseline flow change % @ Full uptake " , 
                       "\n", sep = ""),
         x = 'Limit (%)',
         y = "Mean baseline flow change (%)\n\n") +  
    
    ## Create the themes
    theme_light(base_size = 16) +
    
    theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
          axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size   = 28),
          axis.title.y = element_text(face   = "bold", 
                                      colour = "black", 
                                      size   = 28),
          legend.position  = 'none',
          legend.title     = element_blank(),
          
          panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey")) +
    
    expand_limits(x = 0, y = 0)
  
  
  
  harc_Mean_baseflow_perc_changes <- 
    
    harc_annual_impact_100_Dams_flow_metrics_zero_df %>% 
    
    ggplot(aes(Limit_percent, 
               baseflow_perc_change, 
               color = CHR_Water_Source )) +
    
    geom_line(size = 2, alpha = .8) +
    theme_light(base_size = 16) +
    
    ##
    labs(title = paste("Limit % vs. Mean baseflow change % @ Full uptake " , 
                       "\n", sep = ""),
         x = 'Limit (%)',
         y = "Mean baseflow change (%)\n\n") +  
    
    ## Create the themes
    theme_light(base_size = 16) +
    
    theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
          axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size   = 28),
          axis.title.y = element_text(face   = "bold", 
                                      colour = "black", 
                                      size   = 28),
          legend.position  = 'none',
          legend.title     = element_blank(),
          
          panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey")) +
    
    expand_limits(x = 0, y = 0)
  
  
  
  
  ## 
  harc_Mean_high_spell_change <- harc_annual_impact_100_Dams_flow_metrics_zero_df %>% 
    
    ggplot(aes(Limit_percent, 
               high_spell_perc_change, 
               color = CHR_Water_Source )) +
    
    geom_line(size = 2, alpha = .8) +
    theme_light(base_size = 16) +
    
    ##
    labs(title = paste("Limit % vs. Mean High Spell % change @ Full uptake " , 
                       "\n", sep = ""),
         x = 'Limit (%)',
         y = "Mean High Spell % change\n\n") +  
    
    ## Create the themes
    theme_light(base_size = 16) +
    
    theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
          axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size   = 28),
          axis.title.y = element_text(face   = "bold", 
                                      colour = "black", 
                                      size   = 28),
          legend.position  = 'none',
          legend.title     = element_blank(),
          
          panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey")) +
    
    expand_limits(x = 0, y = 0)
  
  
  
  
  
  ## Low spell
  harc_Mean_low_spell_change <- harc_annual_impact_100_Dams_flow_metrics_zero_df %>% 
    
    ggplot(aes(Limit_percent, 
               low_spell_perc_change, 
               color = CHR_Water_Source )) +
    
    geom_line(size = 2, alpha = .8) +
    theme_light(base_size = 16) +
    
    ##
    labs(title = paste("Limit % vs. Mean Low Spell % change @ Full uptake " , 
                       "\n", sep = ""),
         x = 'Limit (%)',
         y = "Mean Low Spell % change\n\n") +  
    
    ## Create the themes
    theme_light(base_size = 16) +
    
    theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
          axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size   = 28),
          axis.title.y = element_text(face   = "bold", 
                                      colour = "black", 
                                      size   = 28),
          legend.position  = 'none',
          legend.title     = element_blank(),
          
          panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey")) +
    
    expand_limits(x = 0, y = 0)
  
  
  
  
  
  ## High spell
  harc_Mean_seasonality_change <- harc_annual_impact_100_Dams_flow_metrics_zero_df %>% 
    
    ggplot(aes(Limit_percent, 
               seasonal_perc_change, 
               color = CHR_Water_Source )) +
    
    geom_line(size = 2, alpha = .8) +
    theme_light(base_size = 16) +
    
    ##
    labs(title = paste("Limit % vs. Mean Seasonality % change@ Full uptake " , 
                       "\n", sep = ""),
         x = 'Limit (%)',
         y = "Mean Seasonality % change\n\n") +  
    
    ## Create the themes
    theme_light(base_size = 16) +
    
    theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
          axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
          axis.text.y  = element_text(size   = 28),
          axis.title.y = element_text(face   = "bold", 
                                      colour = "black", 
                                      size   = 28),
          legend.position  = 'none',
          legend.title     = element_blank(),
          
          panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                          colour = "grey")) +
    
    expand_limits(x = 0, y = 0)
  
  
  
  ## CV
  # harc_Mean_monthly_CV <- harc_annual_impact_100_Dams_flow_metrics_zero_df %>%
  # 
  #   ggplot(aes(Limit_percent,
  #              monthly.cv,
  #              color = CHR_Water_Source )) +
  # 
  #   geom_line(size  = 2,
  #             alpha = .8) +
  #   theme_light(base_size = 16) +
  # 
  #   ##
  #   labs(title = paste("Limit % vs. Mean monthly CV @ Full uptake " ,
  #                      "\n", sep = ""),
  #        x = 'Limit (%)',
  #        y = "Mean Monthly CV\n\n") +
  # 
  #   ## Create the themes
  #   theme_light(base_size = 16) +
  # 
  #   theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 28),
  #         axis.text.x  = element_text(size   = 28, angle = 0, hjust = 1, vjust = 0.5),
  #         axis.text.y  = element_text(size   = 28),
  #         axis.title.y = element_text(face   = "bold",
  #                                     colour = "black",
  #                                     size   = 28),
  #         legend.position  = 'right',
  #         legend.title     = element_blank(),
  # 
  #         panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
  #                                         colour = "grey"),
  #         panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
  #                                         colour = "grey")) +
  # 
  #   expand_limits(x = 0, y = 0)
  
  
  
  flow_grid <- plot_grid(harc_Mean_ann_flw_perc_changes,
                         harc_Mean_seasonality_change,
                         harc_Mean_high_spell_change,
                         harc_Mean_low_spell_change,
                         nrow       = 2,
                         label_size = 3, 
                         align      = 'hv')
  
  
  png(paste0(hydro_out, 'harc_line_plots.png'),
      20, 16, units = 'in', res = 800)
  plot(flow_grid)
  dev.off()
  
  
  
  ## Save this
  
  
  
  # STEP 5 :: Save data  ----
  
  
  ## Create subset of target reptiles
  
  
  ## Save the CSV - save out all the results to an SQL lite database, or the like
  write_csv(harc_annual_impact_100_Dams_flow_metrics_zero_df,
            paste0(hydro_out, 'harc_annual_impact_100_Dams_flow_metrics.csv'))
  
  message('harc modelling flow charts code successfuly run')
  
  
  
}


#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################