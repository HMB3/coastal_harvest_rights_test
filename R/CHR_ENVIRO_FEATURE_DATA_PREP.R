######################################################################################
###############################  ------ HARVESTABLE RIGHTS ---- ######################
######################################################################################



## ENVIRONMENT SETTINGS =============================================================


# \ 
# 
# This code prepares all the data needed to analyse harvest-able rights
#   
#   
#   \

## To-do

## For all intersects, clip by the defining layer - John's NSW_coastal_water_sources layer

## Check the areas, stream densities, etc.

## Check hierarchy of spatial units





## Function to load or install packages
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, 
                     repos = "https://cran.csiro.au/")
  sapply(pkg, require, character.only = TRUE)
  
}


## Load packages
#devtools::install_github("HMB3/habitatIntersect")
# require(remotes)
# install_github('baddstats/polyclip')

library(habitatIntersect)
library(polyclip)
library(hydroTSM)
library(hydrostats)
library(lubridate)
library(cowplot)
library(tmap)
library(openxlsx)
library(DT)
library(DBI)
library(AHMbook)
library(naniar)
library(arcgisbinding)
library(nngeo)
library(qwraps2)
library(gtsummary)
library(Recocrop)
library(terra)
library(MCMCglmm)
library(officer)
library(flextable)
library(mgcViz)
library(scam)
library(gratia)
library(fuzzyjoin)
library(strex)
library(psycModel)
library(pagedown)


data('sdmgen_packages')
ipak(sdmgen_packages)


## The functions expect these folders,
main_dir             <- paste0(getwd(), "/")
tempdir              <- './TEMP/'
# project_dir          


DPEW_spatial_data    <- './data/DPEW_data_sharing/Spatial_Datasets/'
DPEW_sayers_data     <- './data/DPEW_data_sharing/Spatial_Datasets/Sayers_data/'
HEVAE_data           <- './data/DPEW_data_sharing/HEVAE_tables_Risk_Ratings/'
Wetlands_data        <- './data/enviro/NSW_Wetlands/'
eco_data             <- './data/enviro/eco/'
hydro_data           <- './data/enviro/hydro/'
catchment_data       <- './data/enviro/Catchments/'
economic_data        <- './data/economic/'
cultural_data        <- './data/cultural/'
data_dir             <- './data/combo/'
data_LUT             <- './data/LUT/'


enviro_out           <- './output/enviro/'
hydro_out            <- './output/enviro/hydro/'
conseq_out           <- './output/consequence/'
DC1_out              <- './output/consequence/DC1/'
DC2_out              <- './output/consequence/DC2/'
DC3_out              <- './output/consequence/DC3/'
DC4_out              <- './output/consequence/DC4/'
DC5_out              <- './output/consequence/DC5/'
DC6_out              <- './output/consequence/DC6/'
DC7_out              <- './output/consequence/DC7/'
DC8_out              <- './output/consequence/DC8/'
likely_out           <- './output/likelihood/'
risk_out             <- './output/risk/'


## Database locations - also declare the .GDB files
DPEW_Water_Source_database_loc        <- paste0(catchment_data, 'CHR_DPEW_Input_Water_Source_Data.gpkg')
NSW_Ecological_Layers_database_loc    <- paste0(eco_data,       'CHR_NSW_Input_Ecological_Layers_Data.gpkg')
NSW_Context_Layers_database_loc       <- paste0(economic_data,  'CHR_NSW_Input_Context_Layers_Data.gpkg')
NSW_CHR_WS_Intersections_database_loc <- paste0(conseq_out,     'NSW_CHR_WS_Intersection_Layers.gpkg')
NSW_CHR_WS_HYDRO_Layers_database_loc  <- paste0(hydro_out,      'NSW_CHR_WS_Hydro_Layers.gpkg')
NSW_CHR_WS_CONSEQ_Layers_database_loc <- paste0(conseq_out,     'NSW_CHR_WS_Consequence_Layers.gpkg')
NSW_CHR_WS_LIKELY_Layers_database_loc <- paste0(likely_out,     'NSW_CHR_WS_Likely_Layers.gpkg')
NSW_CHR_WS_RISK_Layers_database_loc   <- paste0(risk_out,       'NSW_CHR_WS_Risk_Layers.gpkg')


## Try and set the raster temp directory to a location not on 
## the partition, to save space
rasterOptions(memfrac = 0.9,
              tmpdir  = tempdir)


terraOptions(memfrac  = 0.9, 
             tempdir  = tempdir) 


message('Use GEOS geometry for sf operations to speed up intersections')
sf_use_s2(FALSE)



save_name   <- 'CHR_VESRION_0'
source('./R/CHR_DATA_ANALYSIS_FUNCTIONS.R')
source('./R/CHR_DATA_PLOTTING_FUNCTIONS.R')
source('./R/CHR_STATS_FUNCTIONS.R')
source('./R/CHR_TABLE_FUNCTIONS.R')




# 2 :: Enviro Feature data ----


if(read_enviro) {
  
  ## 2a Catchment features ----
  message('Reading DPE Feature data')
  DPEW_layers  <- st_layers(dsn = paste0(DPEW_spatial_data, 
                                         "CHR.gdb"))
  
  NSW_unreg_water_sources     <- 
    
    st_read(dsn   = paste0(DPEW_spatial_data, 
                           "CHR.gdb"),
            layer = 'WSP_Surface_Water_Unregulated_1st_July_2022') %>% 
    st_transform(., st_crs(8058)) %>% 
    ensure_multipolygons()
  
  
  NSW_unreg_water_sources_df <- NSW_unreg_water_sources %>% as_tibble() %>% 
    select(-geom, -Shape_Length, -Shape_Area)
  
  
  NSW_Surface_Water_WSP_bound <- 
    
    st_read(dsn   = paste0(DPEW_spatial_data, 
                           "CHR.gdb"),
            layer = 'WSP_Surface_Water_WSP_boundaries_1st_July_2022') %>% 
    st_transform(., st_crs(8058))
  
  NSW_Surface_Water_WSP_bound_df <- NSW_Surface_Water_WSP_bound %>% 
    as_tibble() %>% 
    select(-Shape, -Shape_Length, -Shape_Area)
  
  
  ## Streams with all fields
  NSW_Stream_Order_Coastal <- 
    
    st_read(dsn   = paste0(catchment_data, 
                           "NSW_Coastal_Streams.gpkg"),
            layer = 'NSW_Coastal_Streams') %>% 
    st_transform(., st_crs(8058)) 
  
  
  ## Streams with just the key columns
  NSW_Stream_Order_Length_Coastal <- NSW_Stream_Order_Coastal %>% 
    
    dplyr::mutate(Stream_length_m   = st_length(geom),
                  Stream_length_km  = units::set_units(Stream_length_m , value = km),
                  Stream_length_km  = drop_units(Stream_length_km)) %>% 
    
    dplyr::select(CMA, SUBCMA, HYDRONAME, STRAHLER, Stream_length_km) %>% 
    st_set_precision(units::set_units(10, nm))
  
  
  ## Then we want to group by some of the categories and summarise?
  NSW_Stream_Order_Coastal_df <- NSW_Stream_Order_Length_Coastal %>% as_tibble() %>% 
    select(-geom) 
  
  
  NSW_coastal_water_sources <- st_read(paste0(DPEW_sayers_data, 
                                              'CoastalWaterSourcesMetroMgtZones.shp')) %>% 
    st_transform(., st_crs(8058)) %>% 
    ensure_multipolygons()
  
  
  # Error in CPL_geos_op(“centroid”, x, numeric(0), integer(0), numeric(0), 
  #                       : Evaluation error: ParseException: Unknown WKB type 12.
  
  # Warning message:
  #   In stri_count_regex(string, pattern, opts_regex = opts(pattern)) :
  #   argument is not an atomic vector; coercing
  
  
  NSW_coastal_water_sources_df <- NSW_coastal_water_sources %>% 
    as_tibble() %>% 
    select(-geom)
  
  
  NSW_estuaries                <- 
    st_read(paste0(DPEW_sayers_data, 
                   'Estuaries_Numbers11.shp')) %>% 
    st_transform(., st_crs(8058))
  
  
  NSW_estuaries_df <- NSW_estuaries %>% as_tibble() %>% 
    dplyr::select(EstName, EstComplex, Est_Region, Within_WS) %>% distinct() 
  
  
  NSW_catchments   <- st_read('./data/enviro/Catchments/Catchments_ALB.shp') %>% 
    st_transform(., st_crs(8058))
  
  
  NSW_catchments_df <- NSW_catchments %>% as_tibble() %>%
    select(-SHAPE_Leng, -SHAPE_Area, -geometry)
  
  
  ## Read HARC layers
  HARC_Areas <- st_read(dsn = paste0(hydro_data, 
                                     'HARC/HARC_catchments.shp')) %>% 
    st_transform(., st_crs(8058)) %>% 
    
    dplyr::mutate(m2        = st_area(geometry),
                  Harc_Ha   = units::set_units(m2, value = ha),
                  Harc_Ha   = drop_units(Harc_Ha),
                  
                  Harc_Sqkm = units::set_units(m2, value = km2),
                  Harc_Sqkm = drop_units(Harc_Sqkm)) %>% ensure_multipolygons() 
  
  HARC_Areas_df <- as_tibble(HARC_Areas) %>% 
    select(-geom)
  
  write_csv(HARC_Areas_df , paste0(catchment_data, 'NSW_CHR_HARC_areas.csv'))
  
  
  ## Create LUT of water sources
  ## We want to 
  
  
  ## 1). Subset the Full water source feature layer to just the coastal layer 
  ## 2). Join on the water sharing plans 
  ## 3). Join on the Estuaries
  ## 4). Join on the Catchments if needed
  
  
  ## Subset to the coastal sources 
  ## Manipulate John's layer and WSP ----
  ## Don't do this once Matt provides new layer in 2023
  chr_source_cols <- c(names(NSW_unreg_water_sources), 'UNREGULATE', 'Est_No1')
  
  
  ## For all intersects, clip by the defining layer - John's NSW_coastal_water_sources layer
  NSW_unreg_coastal_water_sources <- st_intersection(NSW_unreg_water_sources, 
                                                     NSW_coastal_water_sources) %>% 
    dplyr::select(all_of(chr_source_cols)) %>% 
    
    ## Now create the new water source column
    ## Also need UNREGULATE, WSP and UNREGULATED_MANAGEMENT_ZONE
    mutate(CHR_Water_Source = UNREGULATE,
           CHR_Water_Source = ifelse(WSP == "GREATER METROPOLITAN (SW)", 
                                     UNREGULATED_MANAGEMENT_ZONE, UNREGULATE))
  
  
  NSW_unreg_coast_ws_df <- NSW_unreg_coastal_water_sources %>% as_tibble() %>% 
    select(-geom)
  
  plot(st_geometry(NSW_unreg_coastal_water_sources))
  plot(st_geometry(NSW_coastal_water_sources))
  
  
  ## Get the intersection - the boundaries have nothing new vs the WSP
  intersect(names(NSW_unreg_coastal_water_sources),
            names(NSW_Surface_Water_WSP_bound))
  
  setdiff(names(NSW_Surface_Water_WSP_bound),
          names(NSW_unreg_coastal_water_sources))
  
  
  
  
  
  ## Create a feature layer for each geography - water source, water sharing, etc.
  ## Dissolve on "UNREGULATED_WATER_SOURCE"
  names(NSW_unreg_coastal_water_sources)
  
  
  NSW_unreg_coastal_water_sources_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    ## Also need UNREGULATE, WSP and UNREGULATED_MANAGEMENT_ZONE
    group_by(MAJOR_CATCH,
             UNREGULATED_WATER_SOURCE, 
             UNREGULATE,
             CHR_Water_Source,
             W_Source_ID, 
             EXTRACTION_MANAGEMENT_UNIT,
             WATER_SHARING_PLAN,
             WSP,
             WSP_ID,
             Est_No1) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% 
    st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() 
  
  
  NSW_unreg_coastal_water_sources_aggregated_df <- 
    NSW_unreg_coastal_water_sources_aggregated %>% as_tibble() %>% 
    dplyr::select(-geometry) %>% distinct()
  
  
  ## This is the starting LUT.
  ## Then we add Jon's Numbering system to this.
  write_csv(NSW_unreg_coastal_water_sources_aggregated_df,
            paste0(catchment_data, 'NSW_unreg_coastal_water_sources_aggregated_df.csv'))
  
  
  ## Update - 
  NSW_UWS_CHR_WS_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, 
    ## W_Source_ID, don't care about the others
    group_by(UNREGULATED_WATER_SOURCE, CHR_Water_Source)  %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  
  NSW_UWS_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(UNREGULATED_WATER_SOURCE) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  
  NSW_CHR_Water_Sources_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(CHR_Water_Source) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% 
    st_make_valid() %>% 
    st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() %>% 
    
    dplyr::mutate(m2       = st_area(geometry),
                  CHR_Ha   = units::set_units(m2, value = ha),
                  CHR_Ha   = drop_units(CHR_Ha),
                  
                  CHR_Sqkm = units::set_units(m2, value = km2),
                  CHR_Sqkm = drop_units(CHR_Sqkm)) %>% 
    
    dplyr::select(CHR_Water_Source,
                  CHR_Ha,
                  CHR_Sqkm) %>% 
    
    filter(CHR_Water_Source != 'Mid Shoalhaven RIver Management Zone')
  
  
  NSW_CHR_Water_Sources_aggregated_df <- 
    NSW_CHR_Water_Sources_aggregated %>% as_tibble() %>% 
    dplyr::select(-geometry)
  
  
  write_csv(NSW_CHR_Water_Sources_aggregated_df,
            paste0(catchment_data, 'NSW_CHR_Water_Sources_aggregated_df.csv'))
  
  
  
  NSW_EST_Num_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## Group by Estuary - how is that different to the separate Estuary file?
    group_by(Est_No1) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  
  NSW_MAJOR_CATCH_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## Group by Estuary - how is that different to the separate Estuary file?
    group_by(MAJOR_CATCH) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  
  NSW_CHR_UWS_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## Group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(UNREGULATED_WATER_SOURCE) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() 
  
  NSW_CHR_UWS_aggregated_df <-
    NSW_CHR_UWS_aggregated %>% as_tibble() %>%
    dplyr::select(-geometry)
  
  
  write_csv(NSW_CHR_UWS_aggregated_df,
            paste0(catchment_data, 'NSW_CHR_UWS_aggregated.csv'))
  
  
  
  NSW_CHR_EMU_aggregated <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(EXTRACTION_MANAGEMENT_UNIT) %>% 
    
    ## This creates slivers, which need to be removed...
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() 
  
  ## WSPs 
  NSW_unreg_coastal_water_sharing_plans <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(WATER_SHARING_PLAN, 
             WSP_ID, 
             WSP_STATUS, 
             WSP, 
             Plan_No,
             START_DATE, 
             END_DATE, 
             COMMENTS) %>% 
    
    summarize() %>% st_make_valid() %>% 
    st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  plot(st_geometry(NSW_unreg_coastal_water_sharing_plans))
  
  
  NSW_unreg_coastal_water_manage_zones <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(UNREGULATED_MANAGEMENT_ZONE, Mgt_Zone_ID) %>% 
    
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  plot(st_geometry(NSW_unreg_coastal_water_manage_zones))
  
  
  
  NSW_unreg_coastal_water_trade_zones <- NSW_unreg_coastal_water_sources %>% 
    
    ## group by UNREGULATED_WATER_SOURCE, W_Source_ID, don't care about the others
    group_by(TRADING_ZONE, EXTRACTION_MANAGEMENT_UNIT) %>% 
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
  plot(st_geometry(NSW_unreg_coastal_water_trade_zones))
  
  
  
  ## Calculate the stream density per water source ----
  ## How to calculate stream density?
  if(ws_stream_density) {
    message('calculate stream densities for Water Sources')
    NSW_water_sources_stream_density_full <- 
      
      st_intersection(st_make_valid(NSW_CHR_Water_Sources_aggregated), 
                      st_make_valid(NSW_Stream_Order_Length_Coastal)) %>% 
      as_tibble() %>% 
      select(-geom)
    gc()
    
    write_csv(NSW_water_sources_stream_density_full, 
              paste0(catchment_data, 'NSW_water_sources_stream_density_full.csv'))
    
    NSW_water_sources_stream_density <- NSW_water_sources_stream_density_full %>% 
      
      group_by(CHR_Water_Source) %>% 
      summarise(Stream_length_km = sum(Stream_length_km), 
                
                ## Don't take the areas from here, join then on from the main CHR layer
                Hectares         = median(CHR_Ha),
                Sqkm             = median(CHR_Sqkm)) %>%
      
      ## 
      dplyr::mutate(Stream_density_Sqkm = Stream_length_km/Sqkm) %>% 
      dplyr::select(CHR_Water_Source, 
                    Stream_length_km, 
                    Stream_density_Sqkm)
    
    write_csv(NSW_water_sources_stream_density,
              paste0(catchment_data, 'NSW_water_sources_stream_density.csv'))
    
  }
  
  if(emu_stream_density) {
    
    message('calculate stream densities for EMUs')
    NSW_EMU_stream_density_full <- 
      
      st_intersection(st_make_valid(NSW_CHR_EMU_aggregated), 
                      st_make_valid(NSW_Stream_Order_Length_Coastal)) %>% 
      as_tibble() %>% 
      select(-geom)
    gc()
    
    write_csv(NSW_EMU_stream_density_full, 
              paste0(catchment_data, 'NSW_EMU_stream_density_full.csv'))
    
    
    NSW_EMU_stream_density <- NSW_EMU_stream_density_full %>% 
      
      group_by(EXTRACTION_MANAGEMENT_UNIT) %>% 
      summarise(EMU_Stream_length_km = sum(Stream_length_km), 
                
                ## Areas are the same for the intersections, so the 
                ## median will do
                EMU_Hectares        = median(EMU_Ha),
                EMU_Sqkm            = median(EMU_Sqkm)) %>%
      
      ## 
      dplyr::mutate(EMU_Stream_density_Sqkm = EMU_Stream_length_km/EMU_Sqkm) %>% 
      dplyr::select(EXTRACTION_MANAGEMENT_UNIT, 
                    EMU_Stream_length_km, 
                    EMU_Stream_density_Sqkm)
    
    write_csv(NSW_EMU_stream_density,
              paste0(catchment_data, 'NNSW_EMU_stream_density.csv'))
    
  }
  
  
  if(harc_stream_density) {
    message('calculate stream densities for Harc Areas')
    NSW_harc_stream_density_full <- 
      
      st_intersection(st_make_valid(HARC_Areas), 
                      st_make_valid(NSW_Stream_Order_Length_Coastal)) %>% 
      as_tibble() %>% 
      select(-geom)
    gc()
    
    write_csv(NSW_harc_stream_density_full, 
              paste0(catchment_data, 'NSW_harc_stream_density_full.csv'))
    
    
    NSW_harc_stream_density <- NSW_harc_stream_density_full %>% 
      
      group_by(GhostName) %>% 
      summarise(Harc_Stream_length_km = sum(Stream_length_km), 
                
                ## Areas are the same for the intersections, so the 
                ## median will do
                Harc_Hectares        = median(Harc_Ha),
                Harc_Sqkm            = median(Harc_Sqkm)) %>%
      
      ## 
      dplyr::mutate(Harc_Stream_density_Sqkm = Harc_Stream_length_km/Harc_Sqkm) %>% 
      dplyr::select(GhostName, 
                    Harc_Stream_length_km, 
                    Harc_Stream_density_Sqkm)
    
    write_csv(NSW_harc_stream_density,
              paste0(catchment_data, 'NSW_harc_stream_density.csv'))
    
    
  }
}


## 2b Ecology features ----
if(eco_layers) {
  
  message('reading ecological layers')
  HEVAE_layers <- st_layers(dsn = paste0(DPEW_spatial_data, 
                                         "HEVAE_Distinctiveness.gdb"))
  
  
  HEVAE_NSW    <- st_read(dsn   = paste0(DPEW_spatial_data, 
                                         "HEVAE_Distinctiveness.gdb"),
                          layer = 'HEVAE_NSW') %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  
  HEVAE_NSW_df     <- HEVAE_NSW %>% as_tibble()
  
  HEVAE_Distinct   <- st_read(dsn   = paste0(DPEW_spatial_data, 
                                             "HEVAE_Distinctiveness.gdb"),
                              layer = 'Distinctiveness_NSW') %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  HEVAE_Distinct_df <- HEVAE_Distinct  %>% as_tibble()
  
  
  NSW_Important_Coastal_Wetlands <- 
    
    st_read(dsn = paste0(Wetlands_data, 
                         "Important_wetlands/important_wetlands.shp")) %>%
    
    st_transform(., st_crs(8058)) %>% 
    filter(STATE == "NSW") %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  NSW_Important_Coastal_Wetlands_df <- NSW_Important_Coastal_Wetlands %>% 
    as_tibble()
  
  
  NSW_Kingsford_Coastal_Wetlands <- 
    
    st_read(dsn = paste0(Wetlands_data, 
                         "NSW_WetlandsMapping_2006/NSW_Wetlands_2006.shp")) %>%
    
    st_transform(., st_crs(8058)) %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  
  
  NSW_Kingsford_Coastal_Wetlands_df <- as_tibble(NSW_Kingsford_Coastal_Wetlands) %>% 
    as_tibble()
  
  
  NSW_RAMSAR_Coastal_Wetlands <- st_read(dsn = paste0(Wetlands_data, 
                                                      "Ramsar/ramsar_wetlands.shp")) %>% 
    st_transform(., st_crs(8058)) %>% 
    filter(STATE == "NSW") %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  
  NSW_RAMSAR_Coastal_Wetlands_df <- NSW_RAMSAR_Coastal_Wetlands %>% as_tibble()
  
  
  plot(st_geometry(NSW_Kingsford_Coastal_Wetlands));
  plot(st_geometry(NSW_Important_Coastal_Wetlands));
  plot(st_geometry(NSW_RAMSAR_Coastal_Wetlands))
  
  
  # NSW_water_sources_HEVAE_Distinct <- 
  #   
  #   ## Need to aggregate this...
  #   st_intersection(NSW_CHR_Water_Sources_aggregated,
  #                   NSW_HEVAE_distinct_layer)
  
  
  ## Eco Health layers ----
  estuary_health_layers <- list.files(paste0(eco_data, 'Health_Risk/'), 
                                      pattern = '.shp', full.names = TRUE)
  
  
  ## Read in the Project feature data here from the inputs geo-database
  EST_health_combined_layers <- estuary_health_layers %>%
    
    ## Pipe the list into lapply
    ## layer <- estuary_health_layers[1]
    lapply(function(layer) {
      
      message('Combine health ', layer)
      
      health_layer <- st_read(dsn = layer) %>% 
        st_transform(., st_crs(8058)) 
      
    }) %>% bind_rows()
  
  
  ## Algal Blooms and ground water
  NSW_CHR_Algal_Blooms_int <- st_read(dsn = paste0(eco_data, 'Alagal_Bloom.shp')) %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_CHR_Water_Sources_aggregated, ] %>% 
    
    st_intersection(., NSW_CHR_Water_Sources_aggregated)
  
  GroundWater_pressure <- st_read(dsn = paste0(eco_data, 'GroundWaterPressure.shp')) %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_CHR_Water_Sources_aggregated, ]
  
  plot(st_geometry(Algal_Blooms_layer))
  plot(st_geometry(GroundWater_pressure))
  plot(st_geometry(NSW_CHR_Water_Sources_aggregated), add = TRUE, col = 'red')
  
  
  ## 2c Protected Areas ----
  
  
  ## CAPAD
  NSW_CAPAD_Marine <- st_read(dsn = paste0(eco_data, "CAPAD_marine_NSW.gpkg")) %>% 
    st_transform(., st_crs(8058)) %>% 
    filter(STATE == "NSW") %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  
  ## IBRA
  NSW_IBRA <- st_read(dsn = paste0(eco_data, "ibra7_subregions.shp")) %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  
  ## Macorphytes
  NSW_MACROPH <- st_read(dsn = paste0(eco_data, 
                                      "./Estuaries_Macrophytes/Data/Estuaries_MacrophyteDetail.shp")) %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_unreg_coastal_water_sources, ]
  
  ## Macorphytes
  NSW_BLUE_CARB <- 
    st_read(dsn = paste0(eco_data, 
                         "./Blue_Carbon/Polygon_NSW Blue Carbon Generation from exisiting mangroves and saltmarsh.shp")) %>% 
    st_transform(., st_crs(8058)) %>% 
    .[NSW_unreg_coastal_water_sources, ] %>% 
    
    ## This might not be needed
    dplyr::rename(BLUE_CARB_Raster = gridcode) 
  
  NSW_BLUE_CARB_agg <- NSW_BLUE_CARB %>% 
    
    ## Anything that has blue carbon potential is sensitive.
    group_by(BLUE_CARB_Raster) %>%
    summarize(BLUE_CARB_Raster = median(BLUE_CARB_Raster)) %>% 
    st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes()
  
}


if(eco_estuary) {
  
  NSW_estuaries <- NSW_estuaries %>% 
    
    st_transform(., st_crs(8058)) %>% 
    rename(Est_No1 = AltNumberi)  %>%
    select(Est_No1) %>% 
    
    ## Calculate the areas
    dplyr::mutate(m2       = st_area(geometry),
                  Est_Ha   = units::set_units(m2, value = ha),
                  Est_Ha   = drop_units(Est_Ha),
                  
                  Est_Sqkm = units::set_units(m2, value = km2),
                  Est_Sqkm = drop_units(Est_Sqkm)) %>% 
    select(Est_No1, Est_Ha, Est_Sqkm)
  
  
  
  # 1. For each of the Reserves, Estuaries and Macrophytes, add a field 
  # in the attribute table called ‘Raster’ and assign all cells a value of ‘1’
  # Not sure what we do with the different categories.
  # EG do they all get a 1, then it just comes down to % covered?
  NSW_CAPAD_Marine_estuary <- NSW_CAPAD_Marine %>% 
    
    dplyr::mutate(CAPAD_Raster = 1) %>% 
    group_by(CAPAD_Raster) %>% 
    
    summarize() %>% st_make_valid() %>% 
    st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() %>% 
    
    dplyr::select(CAPAD_Raster, 
                  geometry) %>% 
    .[NSW_estuaries, ]
  
  
  ## This takes ages, should be pre-prepared
  NSW_MACROPH_estuary <- NSW_MACROPH %>%
    
    dplyr::mutate(MACROPHYTE_Raster = 1) %>% 
    group_by(MACROPHYTE_Raster) %>% 
    
    summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>% 
    nngeo::st_remove_holes() %>% 
    
    dplyr::select(MACROPHYTE_Raster,
                  geometry) %>% 
    .[NSW_estuaries, ]
  
  
  ## Plot data
  # plot(st_geometry(NSW_CHR_estuaries));
  # plot(st_geometry(NSW_CAPAD_Marine_estuary));
  # plot(st_geometry(NSW_MACROPH_estuary));
  # plot(st_geometry(NSW_BLUE_CARB_agg))
  
}




# Save data ----


## Save the spatial data
if(write_enviro) {
  
  ## Create a geo-package of all the input feature data
  ## this can be used to run the analysis, to avoid using .RData files, etc.
  water_source_layer_list <- c('NSW_unreg_water_sources',
                               'NSW_coastal_water_sources',
                               'NSW_estuaries',
                               'NSW_EST_Num_aggregated',
                               'NSW_MAJOR_CATCH_aggregated',
                               'NSW_unreg_coastal_water_sources',
                               'NSW_unreg_coastal_water_sources_aggregated',
                               'NSW_CHR_Water_Sources_aggregated',
                               'NSW_CHR_EMU_aggregated',
                               'NSW_UWS_CHR_WS_aggregated',
                               'NSW_UWS_aggregated',
                               'HARC_Areas',
                               'NSW_unreg_coastal_water_sharing_plans',
                               'NSW_unreg_coastal_water_manage_zones',
                               'NSW_unreg_coastal_water_trade_zones')
  
  
  ## This will include ABS data, etc.
  eco_layer_list <- c('HEVAE_NSW',
                      'HEVAE_Distinct',
                      # 'NSW_water_sources_HEVAE_Distinct',
                      'NSW_Important_Coastal_Wetlands',
                      'NSW_Kingsford_Coastal_Wetlands',
                      'NSW_RAMSAR_Coastal_Wetlands',
                      'NSW_CAPAD_Marine',
                      'NSW_IBRA',
                      'NSW_MACROPH',
                      'NSW_BLUE_CARB',
                      'NSW_CAPAD_Marine_estuary',
                      'NSW_MACROPH_estuary',
                      'NSW_BLUE_CARB_agg',
                      'EST_health_combined_layers',
                      'GroundWater_pressure',
                      'NSW_CHR_Algal_Blooms_int')
  
  if(exists('CHR_DPEW_Input_Data_workbook') == TRUE) {
    message('re-create workbook')
    rm(CHR_DPEW_Input_Data_workbook)
  }
  
  
  ## Check the number of unique features
  message(length(unique(NSW_unreg_water_sources$UNREGULATED_WATER_SOURCE)), ' original WSs')
  message(length(unique(NSW_coastal_water_sources$UNREGULATE)), ' John WSs')
  message(length(unique(NSW_CHR_Water_Sources_aggregated$CHR_Water_Source)), ' MOS coastal WSs')
  message(length(unique(NSW_CHR_EMU_aggregated$EXTRACTION_MANAGEMENT_UNIT)), ' MOS coastal EMUs')
  message(length(unique(NSW_unreg_coastal_water_sharing_plans$WATER_SHARING_PLAN)),         ' MOS coastal WSPs')
  message(length(unique(NSW_unreg_coastal_water_manage_zones$UNREGULATED_MANAGEMENT_ZONE)), ' MOS coastal MZs')
  
  
  message('saving DPEW Input data to Geo-package')
  DPEW_Water_Source_database_loc <- paste0(catchment_data, 'CHR_DPEW_Input_Water_Source_Data.gpkg')
  DPEW_Water_Source_gdb_loc      <- paste0(catchment_data, 'CHR_DPEW_Input_Water_Source_Data.gdb')
  
  NSW_Ecological_Layers_database_loc <- paste0(eco_data, 'CHR_NSW_Input_Ecological_Layers_Data.gpkg')
  NSW_Ecological_Layers_gdb_loc      <- paste0(eco_data, 'CHR_NSW_Input_Ecological_Layers_Data.gdb')
  
  
  if(file.exists(DPEW_Water_Source_database_loc) == TRUE) {
    
    message('re-create geo-package')
    file.remove(DPEW_Water_Source_database_loc)
    
  }
  
  if(eco_layers) {
    
    if(file.exists(NSW_Ecological_Layers_database_loc) == TRUE) {
      message('re-create geo-package')
      file.remove(NSW_Ecological_Layers_database_loc)
      
    }
  }
  
  
  for(layer in water_source_layer_list) {
    
    ## layer <- water_source_layer_list[3]
    File_to_Write <- get(layer) %>% st_cast(., "MULTIPOLYGON")
    
    # gdal_utils(
    #   util = "vectortranslate",
    #   source      = File_to_Write,
    #   destination = DPEW_Water_Source_database_loc, # output format must be specified for GDAL < 2.3
    #   options     = c("-f", "GPKG", "-nlt", "CONVERT_TO_LINEAR")
    # )
    
    #message('writing ', layer, ' to geo database')
    # arc.write(path     = DPEW_Water_Source_gdb_loc,
    #           data     = File_to_Write,
    #           validate = TRUE)
    
    message('writing ', layer, ' to geo-package')
    st_write(File_to_Write,
             dsn        = DPEW_Water_Source_database_loc,
             layer      = layer,
             quiet      = TRUE,
             append     = TRUE)
    
  }
  
  if(eco_layers) {
    
    for(layer in eco_layer_list) {
      
      ## layer <- eco_layer_list[15]
      File_to_Write <- get(layer)
      
      #message('writing ', layer, ' to geo database')
      # arc.write(path     = DPEW_Water_Source_gdb_loc,
      #           data     = File_to_Write,
      #           validate = TRUE)
      
      message('writing ', layer, ' to geo-package')
      st_write(File_to_Write,
               dsn    = NSW_Ecological_Layers_database_loc,
               layer  = layer,
               quiet  = TRUE,
               append = TRUE)
      
    }
    
    con_eco = dbConnect(RSQLite::SQLite(),
                        dbname =  NSW_Ecological_Layers_database_loc)
    
    dbListTables(con_eco)
  }
  
  # Save the tables to an SQL
  con_water = dbConnect(RSQLite::SQLite(),
                        dbname = DPEW_Water_Source_database_loc)
  
  ## see what's in there already:
  dbListTables(con_water)
  
}

gc()




################################################################################################
##################################  ------ TBC ------ ##########################################
################################################################################################