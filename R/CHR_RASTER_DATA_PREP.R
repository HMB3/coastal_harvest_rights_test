#########################################################################################################################
###############################  ------ HARVESTABLE RIGHTS ---- #########################################################
#########################################################################################################################



## ENVIRONMENT SETTINGS =============================================================


# \ 
# 
# This code prepares all the data needed to analyse harvest-able rights
#   
#   
#   \

## To-do

## Check the raster variables with CSIRO
## set the raster resolution for each stack :
## CSIRO : 250m
## BOM : 5km
## LUE : 30m?

## Do the extracts separately in the enviro profile section




# 5 :: Raster data ----


if(read_rasters) {
  
  
  ## 5a CSIRO Climate and Terrain ---- 
  
  ## Raster Names don't auto import anymore...this is every annoying
  csiro_climate_names <- 
    list.files('./data/enviro/Rasters/CSIRO/', pattern = ".tif", full.names = FALSE) %>% 
    gsub('.tif', '', .)
  
  csiro.climate.grids.250m <- raster::stack(
    list.files('./data/enviro/Rasters/CSIRO/', pattern =".tif",    full.names = TRUE))
  
  names(csiro.climate.grids.250m) <- csiro_climate_names
  
  csiro_evap_names <- 
    list.files('./data/enviro/Rasters/CSIRO/PET', pattern = ".tif", full.names = FALSE) %>% 
    gsub('.tif', '', .)
  
  csiro.evap.grids.250m <- raster::stack(
    list.files('./data/enviro/Rasters/CSIRO/PET', pattern =".tif", full.names = TRUE))
  
  names(csiro.evap.grids.250m) <- csiro_evap_names
  
  
  ws_extent_250m = extent(csiro.climate.grids.250m[["Annual_mean_temp"]])
  
  
  ## This will take awhile...save this out
  csiro.climate.grids.250m <- raster::resample(csiro.climate.grids.250m, 
                                               csiro.evap.grids.250m, 
                                               "bilinear", 
                                               extent = ws_extent_250m)
  
  
  csiro.stack.grids.250m <- raster::stack(csiro.climate.grids.250m, 
                                          csiro.evap.grids.250m)
  
  
  ## Harvest rights
  harvest_contours <- raster('./data/enviro/hydro/Farm_Dams/MHRDC_grid_coast_LAM2020.tif') 
  
  
  
  
  ## 5b Edaphic Rasters ----
  
  ## Convert all features to rasters - the numeric need to be translated into 
  ## categorical values again
  ## Turn the land use feature layer into a raster layer


  
  
  
  ## 5c Vegetation Rasters ----
  NSW_forest_30m        <- raster('./data/enviro/Veg/alpsbk_structure_coastal_LAM.tif')
  NSW_forest_250m       <- raster::resample(NSW_forest_30m, 
                                            csiro.climate.grids.250m[["Annual_mean_temp"]], 
                                            "bilinear", 
                                            extent = ws_extent_250m)
  names(NSW_forest_250m) <- c('Forest_Cov')
  
  
  
  ## Macrophyte raster
  NSW_Macrophytes_30m   <- raster('./data/enviro/eco/Blue_Carbon/Raster_NSW_Blue_Carbon_LAM.tif')
  
  
  
  ## Read in Alluvium DEM
  elevation.grids.30m <- raster::stack(
    list.files('./data/enviro/Rasters/terrain', pattern = ".tif", full.names = TRUE))
  
  names(elevation.grids.30m) <- c('Aspect', 'Slope', 'Elevation')
  
  
  elevation.grids.250m     <- raster::resample(elevation.grids.30m, 
                                               csiro.climate.grids.250m[["Annual_mean_temp"]], 
                                               "bilinear", 
                                               extent = ws_extent_250m)
  
  
  water.source.grids.250m <- raster::stack(csiro.climate.grids.250m,
                                           csiro.evap.grids.250m,
                                           elevation.grids.250m,
                                           NSW_forest_250m)
  
  
  ## Make a template raster for the Decision Criteria ----
  mat    <- c(0, 25, 0)
  rclmat <- matrix(mat, ncol = 3, byrow = TRUE)
  
  
  raster_template_250m <- csiro.climate.grids.250m[["Annual_mean_temp"]] %>%
    raster::reclassify(., rclmat, right = FALSE)
  
  ## 
  if(write_rasters) {
    message('saving raster data to R file')
    save.image('CHR_raster_data_environment.RData')
    
  }
  
} else {
  
  
  message('loading raster data from R file')
  load('CHR_raster_data_environment.RData')
  
}

source('./R/CHR_DATA_ANALYSIS_FUNCTIONS.R')
source('./R/CHR_DATA_PLOTTING_FUNCTIONS.R')


## Template raster
mat    <- c(0, 25, 0)
rclmat <- matrix(mat, ncol = 3, byrow = TRUE)


raster_template_250m <- csiro.climate.grids.250m[["Annual_mean_temp"]] %>%
  raster::reclassify(., rclmat, right = FALSE)





#####################################################################################################################
#############################################  ------ TBC ---- ######################################################
#####################################################################################################################