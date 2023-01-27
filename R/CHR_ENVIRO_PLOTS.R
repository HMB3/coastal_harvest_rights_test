#########################################################################################################################
###############################  ------ WATER SOURCE PROFILE ---- #######################################################
#########################################################################################################################


# \ 
# 
# This code analyzes the sensitivity
#   
#   
#   \


## ENVIRONMENT SETTINGS =============================================================


## Function to load or install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos = "https://cran.csiro.au/")
  sapply(pkg, require, character.only = TRUE)
}


## Load packages
#devtools::install_github("HMB3/habitatIntersect")
library(habitatIntersect)
library(hydroTSM)
library(hydrostats)
library(lubridate)
library(flextable)
# library(pryr)
# library(mgcViz)
# library(mvabund)
# library(vegan)
data('sdmgen_packages')
ipak(sdmgen_packages)


## The functions expect these folders,
main_dir             <- paste0(getwd(), "/")
tempdir              <- './TEMP/'

cultural_out         <- './output/cultural/'
economic_out         <- './output/economic/'
enviro_out           <- './output/enviro/'
hydro_out            <- './output/enviro/hydro/'
social_out           <- './output/social/'
combo_out            <- './output/social/'


## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(memfrac = 0.9,
              tmpdir  = tempdir)

terraOptions(memfrac = 0.9, 
             tempdir = tempdir) 


save_name   <- 'HARVEST_RIGHTS_VESRION_0'
source('./R/HARVEST_RIGHTS_DATA_ANALYSIS_FUNCTIONS.R')
source('./R/HARVEST_RIGHTS_DATA_PLOTTING_FUNCTIONS.R')


## DPE/Petter to provide LUT between HARC and water sources
## match harc_annual_impact_100_Dams_flow_metrics_zero_df to match NSW_water_sources


Combined_harc_source_100_10_flow_measures_df <- 
  read_csv(paste0(hydro_out, 'Combined_harc_source_100_10_flow_measures_table.csv'))


Combined_harc_source_100_20_flow_measures_df <- 
  read_csv(paste0(hydro_out, 'Combined_harc_source_100_20_flow_measures_table.csv'))


Combined_harc_source_100_30_flow_measures_df <- 
  read_csv(paste0(hydro_out, 'Combined_harc_source_100_30_flow_measures_table.csv'))


Combined_harc_source_100_50_flow_measures_df <- 
  read_csv(paste0(hydro_out, 'Combined_harc_source_100_50_flow_measures_table.csv'))


all_sources_zonal_stats_table <- 
  read_csv(paste0(hydro_out, 'all_water_sources_zonal_stats_table.csv'))


combined_sources_zonal_stats_table <- 
  read_csv(paste0(hydro_out, 'combined_sources_zonal_stats_table.csv'))


harc_sources_zonal_stats_table <- 
  read_csv(paste0(hydro_out, 'harc_water_sources_zonal_stats_table.csv'))


NSW_Water_Source_LUT <- 
  read_csv(paste0(hydro_out, 'NSW_Water_Sources_LUT.csv'))


## Create lists of these scatterplots, as per the survey


## Create a correlation matrix for all the variables.
## Not as reliable as non-linear, but still useful
all_source_correlations <- rcorr(as.matrix(combined_sources_zonal_stats_table %>%
                                             
                                             dplyr::select(Sq_km,
                                                           Med_Annual_precip,
                                                           Med_pet_ann_5km,
                                                           Med_Slope,
                                                           Med_Elevation,
                                                           Med_Aspect,
                                                           Log_Stream_density,
                                                           Med_AWC,
                                                           Med_BDW,
                                                           Med_SND,
                                                           Med_CLY,
                                                           Med_Forest_Cov)))


all_water_source_correlations_negative <- flattenCorrMatrix(all_source_correlations$r, 
                                                            all_source_correlations$P) %>% 
  as_tibble()     %>% 
  filter(cor < 0) %>% 
  arrange(cor)


all_water_source_correlations_positive <- flattenCorrMatrix(all_source_correlations$r, 
                                                            all_source_correlations$P) %>% 
  as_tibble()     %>% 
  filter(cor > 0) %>% 
  arrange(-cor)



# STEP 5 :: ALL Soure Scatter Plots ----
ALL_source_vars_scatter <- scatter_matrix_grouped_histo(scat_df = 
                                                          
                                                          combined_sources_zonal_stats_table %>%
                                                          
                                                          dplyr::select(Sq_km, 
                                                                        Log_Stream_density, 
                                                                        Med_rain_ann_5km, 
                                                                        Med_Elevation,
                                                                        Med_AWC, 
                                                                        Med_Forest_Cov,
                                                                        HARC),
                                                        
                                                        cols        = 1:6, 
                                                        scat_col    = 'HARC',
                                                        alpha       = 0.7, 
                                                        alignPer    = 0.8,
                                                        upper_size  = 6, 
                                                        lower_size  = 1.5, 
                                                        axis_size   = 16,
                                                        labelSize   = 16,
                                                        title_size  = 18, 
                                                        leg_size    = 20, 
                                                        legend_pos  = 'bottom',
                                                        title       = 'All catchment variables')



png(paste0(hydro_out, 'all_vars_scatter_plot.png'),
    20, 16, units = 'in', res = 600)
ALL_source_vars_scatter
dev.off()



ALL_soure_vars_scatter <- psych::pairs.panels(combined_sources_zonal_stats_table %>%
                                                
                                                ## This is subjective
                                                dplyr::select(Hectares,
                                                              Med_Annual_precip,
                                                              Med_Slope,
                                                              Med_Elevation,
                                                              Log_Stream_density,
                                                              Med_AWC,
                                                              Med_BDW,
                                                              Med_CLY,
                                                              Med_Forest_Cov),
                                              smooth   = FALSE,
                                              method   = "pearson", # correlation method
                                              hist.col = "#00AFBB",
                                              density  = TRUE,      # show density plots
                                              ellipses = FALSE,
                                              cex = 4,
                                              # cex.cor = 2,
                                              cex.labels = 1.5,
                                              lwd = 2,
                                              col = "blue")





# HARC Scatter Plots ----


## Create lists of these scatterplots, as per the survey


## 
HARC_soure_all_vars_scatter <- scatter_matrix_colour_histo(scat_df = 
                                                         
                                                         Combined_harc_source_100_10_flow_measures_df %>%
                                                         
                                                         select(Sq_km, 
                                                                Log_Stream_density, 
                                                                Med_rain_ann_5km, 
                                                                Med_pet_ann_5km,
                                                                Med_Elevation,
                                                                Med_AWC, 
                                                                Med_Forest_Cov),
                                                       
                                                       cols        = 1:7, 
                                                       scat_col    = 'red',
                                                       alpha       = 0.5, 
                                                       alignPer    = 0.8,
                                                       upper_size  = 5, 
                                                       lower_size  = 3, 
                                                       axis_size   = 12,
                                                       labelSize   = 15,
                                                       title_size  = 15, 
                                                       leg_size    = 10, 
                                                       legend_pos  = 'bottom',
                                                       title       = 'All catchment variables')


png(paste0(hydro_out, 'HARC_all_vars_scatter_plot.png'),
    20, 16, units = 'in', res = 600)
HARC_soure_all_vars_scatter
dev.off()




## Need scatterplots to do varaible selection
HARC_soure_gam_vars_scatter <- scatter_matrix_colour_histo(scat_df = Combined_harc_source_100_10_flow_measures_df %>%
                                                         
                                                         select(Impact_mn_ann_flw,
                                                                Max_Annual_precip,
                                                                Max_Aspect,
                                                                Max_BDW,
                                                                Max_SND,
                                                                Max_Forest_Cov,
                                                                Log_Stream_density),
                                                       
                                                       cols        = 1:7, 
                                                       scat_col    = 'red',
                                                       alpha       = 0.5, 
                                                       alignPer    = 0.8,
                                                       upper_size  = 8, 
                                                       lower_size  = 3, 
                                                       axis_size   = 16,
                                                       labelSize   = 16,
                                                       title_size  = 16, 
                                                       leg_size    = 16, 
                                                       legend_pos  = 'bottom',
                                                       title       = 'All catchment variables')

png(paste0(hydro_out, 'HARC_gam_vars_scatter_plot.png'),
    20, 16, units = 'in', res = 600)
HARC_soure_gam_vars_scatter
dev.off()





HARC_vars_Impact_mn_ann_flw <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                     
                                                     ## This is subjective
                                                     dplyr::select(Impact_mn_ann_flw,
                                                                   Hectares,
                                                                   Max_Annual_precip,
                                                                   Max_Slope,
                                                                   Max_Elevation,
                                                                   Log_Stream_density,
                                                                   Max_AWC,
                                                                   Max_BDW,
                                                                   Max_CLY,
                                                                   Med_Forest_Cov),
                                                   smooth   = FALSE,
                                                   method   = "pearson", # correlation method
                                                   hist.col = "#00AFBB",
                                                   density  = TRUE,      # show density plots
                                                   ellipses = FALSE,
                                                   cex = 4,
                                                   # cex.cor = 1.4,
                                                   cex.labels = 1.3,
                                                   lwd = 2,
                                                   col = "blue")


HARC_vars_Mn_basel_flw_perc_change <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                            
                                                            ## This is subjective
                                                            dplyr::select(Mn_basel_flw_perc_change,
                                                                          Hectares,
                                                                          Max_Annual_precip,
                                                                          Max_Slope,
                                                                          Max_Elevation,
                                                                          Med_Aspect,
                                                                          Max_AWC,
                                                                          Max_BDW,
                                                                          Max_CLY,
                                                                          Max_Forest_Cov),
                                                          smooth   = FALSE,
                                                          method   = "pearson", # correlation method
                                                          hist.col = "#00AFBB",
                                                          density  = TRUE,      # show density plots
                                                          ellipses = FALSE,
                                                          cex = 4,
                                                          # cex.cor = 2,
                                                          cex.labels = 1.5,
                                                          lwd = 2,
                                                          col = "blue")


HARC_vars_Mn_ann_flw_perc_change <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                          
                                                          ## This is subjective
                                                          dplyr::select(Mn_ann_flw_perc_change,
                                                                        Hectares,
                                                                        Max_Annual_precip,
                                                                        Max_Slope,
                                                                        Max_Elevation,
                                                                        Med_Aspect,
                                                                        Max_AWC,
                                                                        Max_BDW,
                                                                        Max_CLY,
                                                                        Max_Forest_Cov),
                                                        smooth   = FALSE,
                                                        method   = "pearson", # correlation method
                                                        hist.col = "#00AFBB",
                                                        density  = TRUE,      # show density plots
                                                        ellipses = FALSE,
                                                        cex = 4,
                                                        # cex.cor = 2,
                                                        cex.labels = 1.5,
                                                        lwd = 2,
                                                        col = "blue")


HARC_vars_baseflow_perc_change <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                        
                                                        ## This is subjective
                                                        dplyr::select(baseflow_perc_change,
                                                                      Hectares,
                                                                      Max_Annual_precip,
                                                                      Max_Slope,
                                                                      Max_Elevation,
                                                                      Med_Aspect,
                                                                      Max_AWC,
                                                                      Max_BDW,
                                                                      Max_CLY,
                                                                      Max_Forest_Cov),
                                                      smooth   = FALSE,
                                                      method   = "pearson", # correlation method
                                                      hist.col = "#00AFBB",
                                                      density  = TRUE,      # show density plots
                                                      ellipses = FALSE,
                                                      cex = 4,
                                                      # cex.cor = 2,
                                                      cex.labels = 1.5,
                                                      lwd = 2,
                                                      col = "blue")



HARC_vars_high_spell_perc_change <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                          
                                                          ## This is subjective
                                                          dplyr::select(high_spell_perc_change,
                                                                        Hectares,
                                                                        Max_Annual_precip,
                                                                        Max_Slope,
                                                                        Max_Elevation,
                                                                        Med_Aspect,
                                                                        Max_AWC,
                                                                        Max_BDW,
                                                                        Max_CLY,
                                                                        Max_Forest_Cov),
                                                        smooth   = FALSE,
                                                        method   = "pearson", # correlation method
                                                        hist.col = "#00AFBB",
                                                        density  = TRUE,      # show density plots
                                                        ellipses = FALSE,
                                                        cex = 4,
                                                        # cex.cor = 2,
                                                        cex.labels = 1.5,
                                                        lwd = 2,
                                                        col = "blue")



HARC_vars_low_spell_perc_change <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                         
                                                         ## This is subjective
                                                         dplyr::select(low_spell_perc_change,
                                                                       Hectares,
                                                                       Max_Annual_precip,
                                                                       Max_Slope,
                                                                       Max_Elevation,
                                                                       Med_Aspect,
                                                                       Max_AWC,
                                                                       Max_BDW,
                                                                       Max_CLY,
                                                                       Max_Forest_Cov),
                                                       smooth   = FALSE,
                                                       method   = "pearson", # correlation method
                                                       hist.col = "#00AFBB",
                                                       density  = TRUE,      # show density plots
                                                       ellipses = FALSE,
                                                       cex = 4,
                                                       # cex.cor = 2,
                                                       cex.labels = 1.5,
                                                       lwd = 2,
                                                       col = "blue")



HARC_vars_seasonal_perc_change <- psych::pairs.panels(Combined_harc_source_100_10_flow_measures_df %>%
                                                        
                                                        ## This is subjective
                                                        dplyr::select(seasonal_perc_change,
                                                                      Hectares,
                                                                      Max_Annual_precip,
                                                                      Max_Slope,
                                                                      Max_Elevation,
                                                                      Med_Aspect,
                                                                      Max_AWC,
                                                                      Max_BDW,
                                                                      Max_CLY,
                                                                      Max_Forest_Cov),
                                                      
                                                      smooth   = FALSE,
                                                      method   = "pearson", # correlation method
                                                      hist.col = "#00AFBB",
                                                      density  = TRUE,      # show density plots
                                                      ellipses = FALSE,
                                                      cex = 4,
                                                      # cex.cor = 2,
                                                      cex.labels = 1.5,
                                                      lwd = 2,
                                                      col = "blue")


## Bar plots for each water source ----

## Make a panel of figs for each catchment, need it for all catchments

## Plot the data - do this as a list
plot_zonal_rain_stats <-  single_barchart_order_y(df      = Combined_harc_source_100_10_flow_measures_df,
                                                  title   = 'Rainfall summary',
                                                  caption = '',
                                                  xvar    = 'UNREGULATE',
                                                  yvar    = 'Med_Annual_precip',
                                                  
                                                  ## Set the plot parameters
                                                  tsize     = 35,
                                                  capt_size = 25,
                                                  xsize     = 25,
                                                  ysize     = 25,
                                                  ycol      = 'black',
                                                  col       = "#FF6666",
                                                  lab_size  = 8,
                                                  mar       = 1,
                                                  ylab        = '\nRainfall (mm)\n',
                                                  xlab        = '')


plot_zonal_evapo_stats <-  single_barchart_order_y(df      = Combined_harc_source_100_10_flow_measures_df,
                                                   title   = 'Evapotranspiration summary',
                                                   caption = '',
                                                   xvar    = 'UNREGULATE',
                                                   yvar    = 'Med_pet_ann_5km',
                                                   
                                                   ## Set the plot parameters
                                                   tsize     = 35,
                                                   capt_size = 25,
                                                   xsize     = 25,
                                                   ysize     = 25,
                                                   ycol      = 'black',
                                                   col       = "#FF6666",
                                                   lab_size  = 8,
                                                   mar       = 1,
                                                   ylab        = '\nEvapotranspiration (mm)\n',
                                                   xlab        = '')


plot_zonal_elevation_stats <-  single_barchart_order_y(df      = Combined_harc_source_100_10_flow_measures_df,
                                                       title   = 'Elevation summary',
                                                       caption = '',
                                                       xvar    = 'UNREGULATE',
                                                       yvar    = 'Max_Elevation',
                                                       
                                                       ## Set the plot parameters
                                                       tsize     = 35,
                                                       capt_size = 25,
                                                       xsize     = 25,
                                                       ysize     = 25,
                                                       ycol      = 'black',
                                                       col       = "#FF6666",
                                                       lab_size  = 8,
                                                       mar       = 1,
                                                       ylab        = '\nElevation (m)\n',
                                                       xlab        = '')



#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
