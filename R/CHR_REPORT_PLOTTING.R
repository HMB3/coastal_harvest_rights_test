############################################################################################
#################################  ---- CHR REPORT PLOTS ---- ##############################
############################################################################################


# \ 
# 
# This code combines the CHR measures into one feature layer and table
#
#   \


## To-do

## this layer cannot be subset like this ----
## maybe this need to be the original WS layer, before aggregation
## SO the full join of John's + estuary, with Petter's column


# ws_area <- NSW_CHR_Water_Sources_aggregated_test %>% 
#   dplyr::filter(CHR_Water_Source == ws) 
plot_maps = TRUE
plot_risk = TRUE



## READ DATA =============================================================


if(plot_maps) {
  
  gc()
  # EMU_models <- dam_subset_sort[grepl(EMU_subset_sort[1], dam_subset_sort)] 
  
  
  EMU_dam_feat     <- readRDS(paste0(data_dir, 'EMU_dam_feat.rds'))
  EMU_CHR_Land_use <- readRDS(paste0(data_dir, 'EMU_CHR_Land_use.rds'))
  EMU_Stream_Order <- readRDS(paste0(data_dir, 'EMU_Stream_Order.rds'))
  
  
  ## Just join everything to the one feature layer
  # EMU_dam_feat <- dam_subset[[EMU_models[1]]] %>%
  #   
  #   st_transform(., st_crs(8058)) %>%
  #   .[NSW_CHR_Water_Sources_aggregated_test, ] %>%
  #   st_intersection(., NSW_CHR_Water_Sources_aggregated_test) %>%
  #   filter(LandUseFactor > 0) %>%
  #   mutate(Dams_Area = 'Landuse_Dam') %>%
  #   
  #   ## This creates slivers, which need to be removed...
  #   group_by(Dams_Area) %>%
  #   summarize() %>% st_make_valid() %>% st_buffer(., 0.0) %>%
  #   nngeo::st_remove_holes()
  # 
  # gc()
  
  
  # EMU_CHR_Land_use <-
  #   
  #   st_read(dsn = paste0(data_dir,
  #                        'CHR_NSW_Input_Context_Layers_Data.gpkg'),
  #           layer = 'NSW_land_use_numerics') %>%
  #   .[NSW_CHR_Water_Sources_aggregated_test, ] %>%
  #   st_intersection(., NSW_CHR_Water_Sources_aggregated_test)
  # 
  # gc()
  
  EMU_LUE_Secondary_freq <- EMU_CHR_Land_use %>% as_tibble() %>%
    
    dplyr::select(Secondary) %>%
    tbl_summary(sort = all_categorical() ~ "frequency") %>%
    as_tibble() %>% .[-1,] %>%
    head(., 4)
  names(EMU_LUE_Secondary_freq) <- c('Secondary LUE', 'Count')
  
  gc()
  
  ## Streams with all fields
  # EMU_Stream_Order   <- st_read(dsn   = paste0(catchment_data,
  #                                              "NSW_Coastal_Streams.gpkg"),
  #                               layer = 'NSW_Coastal_Streams') %>%
  #   st_transform(., st_crs(8058)) %>%
  #   .[NSW_CHR_Water_Sources_aggregated_test, ] %>%
  #   st_intersection(., NSW_CHR_Water_Sources_aggregated_test) %>%
  #   filter(STRAHLER > 3)
  
  gc()
  
  
  
  EMU_plot <- 
    
    tm_shape(NSW_CHR_Water_Sources_aggregated_test) +
    tm_polygons(col = "tan1") +
    # tm_borders("black", lwd = 1) +
    
    tm_shape(EMU_dam_feat) +
    tm_polygons(col = "lightblue") +
    # tm_raster("elevation", palette = terrain.colors(10)) +
    
    tm_shape(EMU_Stream_Order) +
    tm_lines(col="blue", lwd="STRAHLER", scale=2, legend.lwd.show = FALSE) +
    
    tm_scale_bar(breaks = c(0, 10, 20), text.size = 1,
                 position=c("left", "bottom")) +
    tm_compass(type = "arrow", size = 2, position = c("left", "top")) +
    tm_layout(legend.outside = TRUE) +
    tm_legend(position = c("left", "bottom"), bg.color = "grey95", frame = TRUE)
  
  gc()
  
  
  
  
  # STEP 1 :: Make Water Source Maps  ----
  
  
  
  ## Update
  water_sources_maps <- WS_test[4:5] %>%
    
    ## Pipe the list into lapply
    #ws = WS_test[4]
    lapply(function(ws) {
      
      message('plotting water source for ', ws)
      
      ## this layer cannot be subset like this ----
      ## maybe this need to be the original WS layer, before aggregation
      ws_area <- NSW_CHR_Water_Sources_aggregated_test %>% 
        dplyr::filter(CHR_Water_Source == ws) 
      
      ## Can't use the WS to subset 
      ws_Stream_Order  <- EMU_Stream_Order %>% 
        .[ws_area, ] #%>% 
      # st_intersection(., ws_area)
      
      ws_dam_feat      <-  EMU_dam_feat %>% 
        .[ws_area, ] #%>% 
      # st_intersection(., ws_area)
      
      
      plot(st_geometry(ws_area))
      plot(st_geometry(ws_dam_feat), 
           border = 'grey', 
           axes = TRUE, add = TRUE)
      plot(st_geometry(EMU_Stream_Order))
      
      # tmap_mode("plot")
      ## tmap mode set to plotting
      WS_plot <- 
        
        tm_shape(ws_area) +
        tm_polygons(col = "tan1") +
        
        tm_shape(ws_dam_feat) +
        tm_polygons(col = 'skyblue3') +
        # tm_raster("elevation", palette = terrain.colors(10)) +
        
        tm_shape(ws_Stream_Order) +
        tm_lines(col="blue", lwd="STRAHLER", scale=2, legend.lwd.show = FALSE) +
        
        tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) +
        tm_compass(type = "arrow", size = 2, position = c("left", "top")) +
        tm_layout(legend.outside = TRUE) +
        tm_legend(position = c("left", "bottom"), bg.color = "grey95", frame = TRUE)
      
      
    }) %>% c(.)
  
  names(water_sources_maps) <- paste0(names(water_sources_maps), WS_test[4:5])
  
  
  ## Plot thematic maps
  DC_1.2_10_pc <- tm_shape(NSW_CHR_Water_Sources_Consequence_sf) +
    
    tm_polygons(col     = "DC_1.2_WET",
                palette = "YlOrRd") +
    
    tm_layout(legend.outside       = FALSE,
              legend.width         = 5,
              panel.label.height   = 5,
              panel.label.size     = 5,
              legend.title.size    = 2, 
              legend.text.size     = 2,
              
              frame                = FALSE,
              main.title           = 'DC1 :: WATER DEPENDENT ECOSYSTEMS', 
              main.title.position  = 'centre',
              main.title.size      = 1.8,
              main.title.fontface  = "bold.italic") 
  
  
  DC_1.3_10_pc <- tm_shape(NSW_CHR_Water_Sources_Consequence_sf) +
    
    tm_polygons(col     = "DC_1.3_MAC",
                palette = "YlOrRd") +
    
    tm_layout(legend.outside       = FALSE,
              legend.width         = 5,
              panel.label.height   = 5,
              panel.label.size     = 5,
              legend.title.size    = 2, 
              legend.text.size     = 2,
              frame                = FALSE,
              main.title           = 'DC2 :: ANTHROPOGENIC PRESSURE', 
              main.title.position  = 'centre',
              main.title.size      = 1.8,
              main.title.fontface  = "bold.italic")
  
  DC_1.4_10_pc <- tm_shape(NSW_CHR_Water_Sources_Consequence_sf) +
    
    tm_polygons(col     = "DC_1.4_CAR",
                palette = "YlOrRd") +
    
    tm_layout(legend.outside       = FALSE,
              legend.width         = 5,
              panel.label.height   = 5,
              panel.label.size     = 5,
              legend.title.size    = 2, 
              legend.text.size     = 2,
              frame                = FALSE,
              main.title           = 'DC5 :: COMMUNITY WELLBEING', 
              main.title.position  = 'centre',
              main.title.size      = 1.8,
              main.title.fontface  = "bold.italic") 
  
  
  DC_1.7_10_pc <- tm_shape(NSW_CHR_Water_Sources_Consequence_sf) +
    
    tm_polygons(col     = "DC_1.7_CAP",
                palette = "YlOrRd") +
    
    tm_layout(legend.outside       = FALSE,
              legend.width         = 5,
              panel.label.height   = 5,
              panel.label.size     = 5,
              legend.title.size    = 2, 
              legend.text.size     = 2,
              frame                = FALSE,
              main.title           = 'DC8 :: ECONOMIC ACTIVITY', 
              main.title.position  = 'centre',
              main.title.size      = 1.8,
              main.title.fontface  = "bold.italic") 
  
  
}


# STEP 2 :: Plot EMU Risk  ----

if(plot_risk) {
  EMU_theme_10 = zero_centred_barchart(highs      = EMU_highs_10,
                                       lows       = EMU_lows_10,
                                       fill_colm  = "Theme",
                                       
                                       scale_cols = c('Neutral'     = 'grey',
                                                      'Opportunity' = 'skyblue3',
                                                      'Risk'        = 'darkorange1'),
                                       
                                       leg_order  = c('Risk',
                                                      'Neutral',
                                                      'Opportunity'),
                                       
                                       title       = '10% CHR option',
                                       
                                       tsize      = 35,
                                       lab_size   = 8,
                                       leg_size   = 25,
                                       ysize      = 25, 
                                       xsize      = 25,
                                       width      = 0.5, 
                                       ymin       = -5.0,
                                       ymax       = 5.5,
                                       high_just  = 0,
                                       low_just   = 1.5)
  
  
  EMU_theme_20 = zero_centred_barchart(highs      = EMU_highs_20,
                                       lows       = EMU_lows_20,
                                       fill_colm  = "Theme",
                                       
                                       scale_cols = c('Neutral'     = 'grey',
                                                      'Opportunity' = 'skyblue3',
                                                      'Risk'        = 'darkorange1'),
                                       
                                       leg_order  = c('Risk',
                                                      'Neutral',
                                                      'Opportunity'),
                                       
                                       title       = '20% CHR option',
                                       
                                       tsize      = 35,
                                       lab_size   = 8,
                                       leg_size   = 25,
                                       ysize      = 25, 
                                       xsize      = 25,
                                       width      = 0.5, 
                                       ymin       = -5.0,
                                       ymax       = 5.5,
                                       high_just  = 0,
                                       low_just   = 1.5)
  
  
  EMU_theme_30 = zero_centred_barchart(highs      = EMU_highs_30,
                                       lows       = EMU_lows_30,
                                       fill_colm  = "Theme",
                                       
                                       scale_cols = c('Neutral'     = 'grey',
                                                      'Opportunity' = 'skyblue3',
                                                      'Risk'        = 'darkorange1'),
                                       
                                       leg_order  = c('Risk',
                                                      'Neutral',
                                                      'Opportunity'),
                                       
                                       title       = '30% CHR option',
                                       
                                       tsize      = 35,
                                       lab_size   = 8,
                                       leg_size   = 25,
                                       ysize      = 25, 
                                       xsize      = 25,
                                       width      = 0.5, 
                                       ymin       = -5.0,
                                       ymax       = 5.5,
                                       high_just  = 0,
                                       low_just   = 1.5)
  
  
  EMU_theme_40 = zero_centred_barchart(highs      = EMU_highs_40,
                                       lows       = EMU_lows_40,
                                       fill_colm  = "Theme",
                                       
                                       scale_cols = c('Neutral'     = 'grey',
                                                      'Opportunity' = 'skyblue3',
                                                      'Risk'        = 'darkorange1'),
                                       
                                       leg_order  = c('Risk',
                                                      'Neutral',
                                                      'Opportunity'),
                                       
                                       title       = '40% CHR option',
                                       
                                       tsize      = 35,
                                       lab_size   = 8,
                                       leg_size   = 25,
                                       ysize      = 25, 
                                       xsize      = 25,
                                       width      = 0.5, 
                                       ymin       = -5.0,
                                       ymax       = 5.5,
                                       high_just  = 0,
                                       low_just   = 1.5)
  
  
  EMU_theme_50 = zero_centred_barchart(highs      = EMU_highs_50,
                                       lows       = EMU_lows_50,
                                       fill_colm  = "Theme",
                                       
                                       scale_cols = c('Neutral'     = 'grey',
                                                      'Opportunity' = 'skyblue3',
                                                      'Risk'        = 'darkorange1'),
                                       
                                       leg_order  = c('Risk',
                                                      'Neutral',
                                                      'Opportunity'),
                                       
                                       title       = '50% CHR option',
                                       
                                       tsize      = 35,
                                       lab_size   = 8,
                                       leg_size   = 25,
                                       ysize      = 25, 
                                       xsize      = 25,
                                       width      = 0.5, 
                                       ymin       = -5.0,
                                       ymax       = 5.5,
                                       high_just  = 0,
                                       low_just   = 1.5)
  
  
  
  
  
  # STEP 2 :: Plot WS DC Risk  ----
  
  
  
  ## WS1 ----
  WS1_DC_10 = zero_centred_barchart(highs      = WS1_highs_10,
                                    lows       = WS1_lows_10,
                                    fill_colm  = "Criteria",
                                    
                                    scale_cols = c('Neutral'     = 'grey',
                                                   'Opportunity' = 'skyblue3',
                                                   'Risk'        = 'darkorange1'),
                                    
                                    leg_order  = c('Risk',
                                                   'Neutral',
                                                   'Opportunity'),
                                    
                                    title       = '10% CHR option',
                                    
                                    tsize      = 35,
                                    lab_size   = 8,
                                    leg_size   = 25,
                                    ysize      = 15, 
                                    xsize      = 25,
                                    width      = 0.5, 
                                    ymin       = -5.0,
                                    ymax       = 5.5,
                                    high_just  = 0,
                                    low_just   = 1.5)
  
  
  WS1_DC_30 = zero_centred_barchart(highs      = WS1_highs_30,
                                    lows       = WS1_lows_30,
                                    fill_colm  = "Criteria",
                                    
                                    scale_cols = c('Neutral'     = 'grey',
                                                   'Opportunity' = 'skyblue3',
                                                   'Risk'        = 'darkorange1'),
                                    
                                    leg_order  = c('Risk',
                                                   'Neutral',
                                                   'Opportunity'),
                                    
                                    title       = '30% CHR option',
                                    
                                    tsize      = 35,
                                    lab_size   = 8,
                                    leg_size   = 25,
                                    ysize      = 15, 
                                    xsize      = 25,
                                    width      = 0.5, 
                                    ymin       = -5.0,
                                    ymax       = 5.5,
                                    high_just  = 0,
                                    low_just   = 1.5)
  
  
  WS1_DC_50 = zero_centred_barchart(highs      = WS1_highs_50,
                                    lows       = WS1_lows_50,
                                    fill_colm  = "Criteria",
                                    
                                    scale_cols = c('Neutral'     = 'grey',
                                                   'Opportunity' = 'skyblue3',
                                                   'Risk'        = 'darkorange1'),
                                    
                                    leg_order  = c('Risk',
                                                   'Neutral',
                                                   'Opportunity'),
                                    
                                    title       = '50% CHR option',
                                    
                                    tsize      = 35,
                                    lab_size   = 8,
                                    leg_size   = 25,
                                    ysize      = 15, 
                                    xsize      = 25,
                                    width      = 0.5, 
                                    ymin       = -5.0,
                                    ymax       = 5.5,
                                    high_just  = 0,
                                    low_just   = 1.5)
  
  
  ## WS2 ----
  WS2_DC_10 = zero_centred_barchart(highs      = WS2_highs_10,
                                    lows       = WS2_lows_10,
                                    fill_colm  = "Criteria",
                                    
                                    scale_cols = c('Neutral'     = 'grey',
                                                   'Opportunity' = 'skyblue3',
                                                   'Risk'        = 'darkorange1'),
                                    
                                    leg_order  = c('Risk',
                                                   'Neutral',
                                                   'Opportunity'),
                                    
                                    title       = '10% CHR option',
                                    
                                    tsize      = 35,
                                    lab_size   = 8,
                                    leg_size   = 25,
                                    ysize      = 15, 
                                    xsize      = 25,
                                    width      = 0.5, 
                                    ymin       = -5.0,
                                    ymax       = 5.5,
                                    high_just  = 0,
                                    low_just   = 1.5)
  
  
  WS2_DC_30 = zero_centred_barchart(highs      = WS2_highs_30,
                                    lows       = WS2_lows_30,
                                    fill_colm  = "Criteria",
                                    
                                    scale_cols = c('Neutral'     = 'grey',
                                                   'Opportunity' = 'skyblue3',
                                                   'Risk'        = 'darkorange1'),
                                    
                                    leg_order  = c('Risk',
                                                   'Neutral',
                                                   'Opportunity'),
                                    
                                    title       = '30% CHR option',
                                    
                                    tsize      = 35,
                                    lab_size   = 8,
                                    leg_size   = 25,
                                    ysize      = 15, 
                                    xsize      = 25,
                                    width      = 0.5, 
                                    ymin       = -5.0,
                                    ymax       = 5.5,
                                    high_just  = 0,
                                    low_just   = 1.5)
  
  
  WS2_DC_50 = zero_centred_barchart(highs      = WS2_highs_50,
                                    lows       = WS2_lows_50,
                                    fill_colm  = "Criteria",
                                    
                                    scale_cols = c('Neutral'     = 'grey',
                                                   'Opportunity' = 'skyblue3',
                                                   'Risk'        = 'darkorange1'),
                                    
                                    leg_order  = c('Risk',
                                                   'Neutral',
                                                   'Opportunity'),
                                    
                                    title       = '50% CHR option',
                                    
                                    tsize      = 35,
                                    lab_size   = 8,
                                    leg_size   = 25,
                                    ysize      = 15, 
                                    xsize      = 25,
                                    width      = 0.5, 
                                    ymin       = -5.0,
                                    ymax       = 5.5,
                                    high_just  = 0,
                                    low_just   = 1.5)
  
  
}

############################################################################################
#################################  ---- CHR REPORT PLOTS ---- ##############################
############################################################################################