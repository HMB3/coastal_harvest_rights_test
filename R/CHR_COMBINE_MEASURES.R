############################################################################################
##############################  ---- CHR REPORT DATA  ---- ###############################
############################################################################################


# \ 
# 
# This code combines the CHR measures into one feature layer and table
#
#   \


## Do each criteria separately.
## Then have a separate script for the combination





## Add the columns
## Some of the rules are in consequence files days 
## add weightings for each measure 1.1 = 0.6, remaining 40 % split across the remaining
## etc



# STEP 1 :: Create Master tables  ----
CHR_DC_master_table <- read_csv(paste0(conseq_out, 
                                       'NSW_CHR_WS_DC_CONSEQUENCE_SCORES_df.csv'))


enviro_theme   <- c("DC_1.1_HEVAE",	
                    "DC_1.2_WET",	
                    "DC_1.3_MACRO",	
                    "DC_1.4_CARBON",	
                    "DC_1.7_CAPAD",	
                    "DC_2.2_AV_EXTR_LOW",	
                    "DC_3.1_EST_HEALTH",	
                    "DC_3.2_ALGAL_low")


cultural_theme <- c("DC_4.1_NTT",	
                    "DC_4.2_AIMS")


social_theme   <- c("DC_5.1_SEIFA",	
                    "DC_5.2_UNEMP",	
                    "DC_5.5_FISH",	
                    "DC_5.6_BOAT")


economic_theme <- c("DC_6.1_TOUR", 
                    "DC_6.2_AQUACULT",
                    "DC_7.1_WATER_Ha", 
                    "DC_7.2_WATER_Pop",
                    "DC_7.1_WATER_Ha", 
                    "DC_7.2_WATER_Pop",
                    "DC_8.1_WATER_USE",	
                    "DC_8.2_GROSS",	
                    "DC_8.3_AG_EMP")




## create the values for theme and DC tables for the EMU
EMU_Option_10_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 3.2,  sd = 2, lower = -5, upper = 5)   %>% round()
EMU_Option_20_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 2.8,  sd = 2, lower = -5, upper = 5)   %>% round()
EMU_Option_30_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 1.2,  sd = 2, lower = -5, upper = 5)   %>% round()
EMU_Option_40_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = -0.5, sd = 2, lower = -5, upper = 5)   %>% round()
EMU_Option_50_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = -2.5, sd = 2, lower = -5, upper = 5)   %>% round()


## create the values for DC tables for the WS - example 1
WS1_Option_10_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 3.8,  sd = 1.7, lower = -5, upper = 5)  %>% round()
WS1_Option_20_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 3.0,  sd = 1.7, lower = -5, upper = 5)  %>% round()
WS1_Option_30_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 0.0,  sd = 1.7, lower = -5, upper = 5)  %>% round()
WS1_Option_40_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = -1.2, sd = 1.7, lower = -5, upper = 5)  %>% round()
WS1_Option_50_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = -2.7, sd = 1.7, lower = -5, upper = 5)  %>% round()


WS2_Option_10_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 3.0,  sd = 1.5, lower = -5, upper = 5)  %>% round()
WS2_Option_20_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 2.2,  sd = 1.5, lower = -5, upper = 5)  %>% round()
WS2_Option_30_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = 0.5,  sd = 1.5, lower = -5, upper = 5)  %>% round()
WS2_Option_40_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = -1.5, sd = 1.5, lower = -5, upper = 5)  %>% round()
WS2_Option_50_limit <- rtnorm(n = nrow(CHR_DC_master_table), mean = -3.2, sd = 1.5, lower = -5, upper = 5)  %>% round()



## Now bind the values on
CHR_DC_EMU_master_table <- CHR_DC_master_table %>% 
  
  ## Update here
  mutate(Risk_Current = EMU_Option_10_limit,
         Risk_20pc    = EMU_Option_20_limit,
         Risk_30pc    = EMU_Option_30_limit,
         Risk_40pc    = EMU_Option_40_limit,
         Risk_50pc    = EMU_Option_50_limit) 


## Now bind the values on
CHR_DC_WS1_master_table <- CHR_DC_master_table %>% 
  
  ## Update here
  mutate(Risk_Current = WS1_Option_10_limit,
         Risk_20pc    = WS1_Option_20_limit,
         Risk_30pc    = WS1_Option_30_limit,
         Risk_40pc    = WS1_Option_40_limit,
         Risk_50pc    = WS1_Option_50_limit) 


CHR_DC_WS2_master_table <- CHR_DC_master_table %>% 
  
  ## Update here
  mutate(Risk_Current = WS2_Option_10_limit,
         Risk_20pc    = WS2_Option_20_limit,
         Risk_30pc    = WS2_Option_30_limit,
         Risk_40pc    = WS2_Option_40_limit,
         Risk_50pc    = WS2_Option_50_limit) 






# STEP 2 :: Aggregate tables  ----


EMU_Risk_themes <- CHR_DC_EMU_master_table %>% 
  
  ## group by theme
  group_by(Theme) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE) 


EMU_Risk_DCs <- CHR_DC_EMU_master_table %>% 
  
  ## group by theme
  group_by(Criteria) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)


WS1_Risk_DCs <- CHR_DC_WS1_master_table %>% 
  
  ## group by theme
  group_by(Criteria) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)



WS2_Risk_DCs <- CHR_DC_WS2_master_table %>% 
  
  ## group by theme
  group_by(Criteria) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)





# STEP 3 :: Create plotting tables  ----


## Everything from here down should be in a function.
## Can use the sym!! etc


## EMU plot tabs ----
## For the likert graphs, need to warp the tables and make factors#
EMU_Risk_themes_plot_10 <- EMU_Risk_themes %>% select(Theme, Risk_Current) %>% 
  rename(Risk = Risk_Current) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


EMU_Risk_themes_plot_20 <- EMU_Risk_themes %>% select(Theme, Risk_20pc) %>% 
  rename(Risk  = Risk_20pc) %>% 
  mutate(Class = ifelse(Risk >0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


EMU_Risk_themes_plot_30 <- EMU_Risk_themes %>% select(Theme, Risk_30pc) %>% 
  rename(Risk  = Risk_30pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


EMU_Risk_themes_plot_40 <- EMU_Risk_themes %>% select(Theme, Risk_40pc) %>% 
  rename(Risk  = Risk_40pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


EMU_Risk_themes_plot_50 <- EMU_Risk_themes %>% select(Theme, Risk_50pc) %>% 
  rename(Risk  = Risk_50pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))




## Create the positive and negative bars
EMU_highs_10 <- EMU_Risk_themes_plot_10  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
EMU_lows_10  <- EMU_Risk_themes_plot_10  %>% dplyr::filter(Risk < 0) %>% na.omit()

EMU_highs_20 <- EMU_Risk_themes_plot_20  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
EMU_lows_20  <- EMU_Risk_themes_plot_20  %>% dplyr::filter(Risk < 0) %>% na.omit()

EMU_highs_30 <- EMU_Risk_themes_plot_30  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
EMU_lows_30  <- EMU_Risk_themes_plot_30  %>% dplyr::filter(Risk < 0) %>% na.omit()

EMU_highs_40 <- EMU_Risk_themes_plot_40  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
EMU_lows_40  <- EMU_Risk_themes_plot_40  %>% dplyr::filter(Risk < 0) %>% na.omit()

EMU_highs_50 <- EMU_Risk_themes_plot_50  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
EMU_lows_50  <- EMU_Risk_themes_plot_50  %>% dplyr::filter(Risk < 0) %>% na.omit()


## Re-order the lows, as they are negative in order
EMU_lows_10$Theme <- forcats::fct_rev(EMU_lows_10$Theme)
EMU_lows_20$Theme <- forcats::fct_rev(EMU_lows_20$Theme)
EMU_lows_30$Theme <- forcats::fct_rev(EMU_lows_30$Theme)
EMU_lows_40$Theme <- forcats::fct_rev(EMU_lows_40$Theme)
EMU_lows_50$Theme <- forcats::fct_rev(EMU_lows_50$Theme)




## WS plot tabs ----
## For the likert graphs, need to warp the tables and make factors#
WS1_Risk_DCs_plot_10 <- WS1_Risk_DCs %>% select(Criteria, Risk_Current) %>% 
  rename(Risk = Risk_Current) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS1_Risk_DCs_plot_20 <- WS1_Risk_DCs %>% select(Criteria, Risk_20pc) %>% 
  rename(Risk  = Risk_20pc) %>% 
  mutate(Class = ifelse(Risk >0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS1_Risk_DCs_plot_30 <- WS1_Risk_DCs %>% select(Criteria, Risk_30pc) %>% 
  rename(Risk  = Risk_30pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS1_Risk_DCs_plot_40 <- WS1_Risk_DCs %>% select(Criteria, Risk_40pc) %>% 
  rename(Risk  = Risk_40pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS1_Risk_DCs_plot_50 <- WS1_Risk_DCs %>% select(Criteria, Risk_50pc) %>% 
  rename(Risk  = Risk_50pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))




WS2_Risk_DCs_plot_10 <- WS2_Risk_DCs %>% select(Criteria, Risk_Current) %>% 
  rename(Risk = Risk_Current) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS2_Risk_DCs_plot_20 <- WS2_Risk_DCs %>% select(Criteria, Risk_20pc) %>% 
  rename(Risk  = Risk_20pc) %>% 
  mutate(Class = ifelse(Risk >0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS2_Risk_DCs_plot_30 <- WS2_Risk_DCs %>% select(Criteria, Risk_30pc) %>% 
  rename(Risk  = Risk_30pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS2_Risk_DCs_plot_40 <- WS2_Risk_DCs %>% select(Criteria, Risk_40pc) %>% 
  rename(Risk  = Risk_40pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))


WS2_Risk_DCs_plot_50 <- WS2_Risk_DCs %>% select(Criteria, Risk_50pc) %>% 
  rename(Risk  = Risk_50pc) %>% 
  mutate(Class = ifelse(Risk > 0, 'Opportunity', 'Risk')) %>% 
  mutate(Class = ifelse(Risk < 0, 'Risk', 'Opportunity')) %>% 
  mutate(Class = as.factor(Class))



## Create the positive and negative bars
WS1_highs_10 <- WS1_Risk_DCs_plot_10  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS1_lows_10  <- WS1_Risk_DCs_plot_10  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS1_highs_20 <- WS1_Risk_DCs_plot_20  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS1_lows_20  <- WS1_Risk_DCs_plot_20  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS1_highs_30 <- WS1_Risk_DCs_plot_30  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS1_lows_30  <- WS1_Risk_DCs_plot_30  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS1_highs_40 <- WS1_Risk_DCs_plot_40  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS1_lows_40  <- WS1_Risk_DCs_plot_40  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS1_highs_50 <- WS1_Risk_DCs_plot_50  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS1_lows_50  <- WS1_Risk_DCs_plot_50  %>% dplyr::filter(Risk < 0) %>% na.omit()



WS2_highs_10 <- WS2_Risk_DCs_plot_10  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS2_lows_10  <- WS2_Risk_DCs_plot_10  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS2_highs_20 <- WS2_Risk_DCs_plot_20  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS2_lows_20  <- WS2_Risk_DCs_plot_20  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS2_highs_30 <- WS2_Risk_DCs_plot_30  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS2_lows_30  <- WS2_Risk_DCs_plot_30  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS2_highs_40 <- WS2_Risk_DCs_plot_40  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS2_lows_40  <- WS2_Risk_DCs_plot_40  %>% dplyr::filter(Risk < 0) %>% na.omit()

WS2_highs_50 <- WS2_Risk_DCs_plot_50  %>% dplyr::filter(Risk > 0 | Risk == 0) %>% na.omit()
WS2_lows_50  <- WS2_Risk_DCs_plot_50  %>% dplyr::filter(Risk < 0) %>% na.omit()





## Re-order the lows, as they are negative in order
WS1_lows_10$Criteria <- forcats::fct_rev(WS1_lows_10$Criteria)
WS1_lows_20$Criteria <- forcats::fct_rev(WS1_lows_20$Criteria)
WS1_lows_30$Criteria <- forcats::fct_rev(WS1_lows_30$Criteria)
WS1_lows_40$Criteria <- forcats::fct_rev(WS1_lows_40$Criteria)
WS1_lows_50$Criteria <- forcats::fct_rev(WS1_lows_50$Criteria)


WS2_lows_10$Criteria <- forcats::fct_rev(WS2_lows_10$Criteria)
WS2_lows_20$Criteria <- forcats::fct_rev(WS2_lows_20$Criteria)
WS2_lows_30$Criteria <- forcats::fct_rev(WS2_lows_30$Criteria)
WS2_lows_40$Criteria <- forcats::fct_rev(WS2_lows_40$Criteria)
WS2_lows_50$Criteria <- forcats::fct_rev(WS2_lows_50$Criteria)



######################################################################################
###########################  ------ CHR RIGHTS ANALYSIS ---- #########################
######################################################################################
