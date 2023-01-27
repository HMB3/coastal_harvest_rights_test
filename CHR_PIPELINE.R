######################################################################################
###########################  ------ CHR RIGHTS ANALYSIS ---- #########################
######################################################################################



# \ 
# 
# This code prepares all the data for the CHR assessment, and runs the analyses \
#   
# \



## To-do ----

## Spatial and conceptual coupling of consequence and likelihood - rules come from different part 
## of likelihood uniqueness - .


## Re-format HEVAE Risk and Threatened species tables
## Check flow metrics
## Check all feature data 
## Check all decision points
## Integrate LUT with processing code : which variables will users change?


## Clear environment
rm(list = ls())
options(warn = 0)


## These flags control what is run
## some could be in the LUT (excel workbook).
## If the data prep and analyses is already done, the initial flags can be false
## These steps are the initial data prep.
read_enviro         <- FALSE  ## read in enviro spatial (feature) inputs from disk and pre-process?
ws_stream_density   <- FALSE
emu_stream_density  <- FALSE
harc_stream_density <- FALSE
eco_layers          <- FALSE  ## read in enviro spatial (feature) inputs from disk and pre-process?
eco_estuary         <- FALSE
write_excel         <- FALSE  ## save spatial data tables to excel?
write_enviro        <- FALSE  ## save processed spatial feature inputs to geo-database?
read_context        <- FALSE  ## read in enviro spatial (feature) inputs from disk and pre-process?
land_use            <- FALSE  ## read in land use data and pre-process?
write_context       <- FALSE 


read_rasters        <- FALSE  ## read in enviro raster grids from disk and pre-process?
write_rasters       <- FALSE  ## save processed enviro raster grids to disk?
rasterize           <- FALSE
conseq_inter        <- FALSE
enviro_summary      <- FALSE  ## calculate enviro profiles of each EMU/water source?
dam_list            <- FALSE
dam_areas           <- FALSE  ## Aggregate the DEPW results of farm dam area analyses?
harc_models         <- FALSE  ## Aggregate the results of the HARC models for trial water sources?
ordination          <- FALSE
harc_predict        <- FALSE


## These steps are more subjective, where the areas come in
likely              <- FALSE  ## Calculate likelihood of impact from DPEW-modelled change in gauge flow 
flow_metrics        <- FALSE  ## Calculate flow metrics for DPEW-modelled gauges
consequence         <- FALSE  ## Calculate consequence values for all measures, DCs and themes
risk                <- FALSE  ## Calculate combined risk by multiplying 
knit_reports        <- FALSE  ## Knit reports for each EMU (and it's water sources) to .PDF. 


## The above flags are used inside the scripts below 




# 1 :: LOAD DATA ----


## These scripts set the environments, reads in all the raster and feature data, etc. 
source('./R/CHR_ENVIRO_FEATURE_DATA_PREP.R')
source('./R/CHR_SOT_DATA_PREP.R')
source('./R/CHR_CONTEXT_FEATURE_DATA_PREP.R')


source('./R/CHR_RASTER_DATA_PREP.R')
source('./R/CHR_CONTEXT_RASTER_DATA_PREP.R')
source('./R/CHR_CONSEQ_INTERSECTS.R')




# 2 :: CALCULATE LIKELIHOOD  ---- 


## These scripts calculate the hydro-logical components and likely 
## impacts of harvest options
if(likely) {
  
  source('./R/CHR_WATER_SOURCE_ENVIRO_PROFILE.R')
  source('./R/CHR_FARM_DAMS.R')
  
  source('./R/CHR_WATER_SOURCE_HARC_MODELS.R')
  source('./R/CHR_WATER_SOURCE_FLOW_PREDICTION.R') 
  source('./R/CHR_LIKELIHOOD_CALC.R')
  
}



# 3 :: CALCULATE FLOW METRICS ---- 


# 4 :: CALCULATE CONSEQUENCE ---- 


## These scripts calculate the consequence measures for each Decision Criteria
## They are split up by length - some DC's are too big to combine in one script...
if(consequence) {
  
  ## 
  source('./R/CHR_DC_1.1_HEVAE.R')             ## Calculate the first two Enviro measures
  source('./R/CHR_DC_1.3_SEAGRASS.R')          ## Calculate the remaining Enviro measures
  source('./R/CHR_DC_2.1_WATER_EXTRACTION.R')  ## Calculate the extraction measures  
  source('./R/CHR_DC_3.1_ESTUARY_HEALTH.R')    ## Calculate the estuary health measures                               
  source('./R/CHR_DC_4.1_INDIGENOUS.R')        ## Calculate the indigenous measures
  source('./R/CHR_DC_5.1_SEIFA.R')             ## Calculate the ABS demographic measures 
  source('./R/CHR_DC_6.1_BUSINESS.R')          ## Calculate the business measures 
  source('./R/CHR_DC_7.1_WATER_DEMAND.R')      ## Calculate the water demand measures 
  source('./R/CHR_DC_8.1_WATER_ECON.R')        ## Calculate the water econonomic measures
  # source('./R/CHR_DC_9.1_WATER_ECON.R')      ## Estimate the uncertainty of each measure
  source('./R/CHR_COMBINE_ALL_DC_MEASURES.R')  ## Combine all measures
  
}




# 5 :: CALCULATE COMBINED RISK ---- 
source('./R/CHR_COMBINE_MEASURES.R')
source('./R/CHR_COMBINE_RISK_MAPS.R')
source('./R/CHR_REPORT_PLOTTING.R')
source('./R/CHR_REPORT_TABLES.R')




# 6 :: KNIT EMU/WS REPORTS ---- 


## Once all the data have been processed, and the risk
## calculations have been run for all measures, criterias and themes 
## for all EMU's and water sources, we can summarise the results for 
## each EMU. This is done using markdown to render a .PDF report for
## each EMU, and each containing water source, that summarises the risks
## associated with each harvest-able rights limits option.
source('./R/CHR_EMU_REPORTS.R')



# 7 :: SAVE RESULTS TO DATABASE ---- 


## 
# source('./R/CHR_ARHIVE_RESULTS.R')





######################################################################################
###########################  ------ CHR RIGHTS ANALYSIS ---- #########################
######################################################################################