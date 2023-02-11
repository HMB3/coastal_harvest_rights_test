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




# 1). Plot EMU Risk  ----

if(plot_risk) {
  
  EMU_theme_10 <- zero_centred_barchart(highs      = EMU_highs_10,
                                        lows       = EMU_lows_10,
                                        fill_colm  = "Theme",
                                        
                                        scale_cols = c('Neutral'     = 'grey',
                                                       'Opportunity' = 'skyblue3',
                                                       'Risk'        = 'darkorange1'),
                                        
                                        leg_order  = c('Risk',
                                                       'Neutral',
                                                       'Opportunity'),
                                        
                                        title      = '10% CHR option',
                                        
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
  
  
  EMU_theme_20 <- zero_centred_barchart(highs      = EMU_highs_20,
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
  
  
  EMU_theme_30 <- zero_centred_barchart(highs      = EMU_highs_30,
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
  
  
  EMU_theme_40 <- zero_centred_barchart(highs      = EMU_highs_40,
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
  
  
  EMU_theme_50 <- zero_centred_barchart(highs      = EMU_highs_50,
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
  
  
  
  
  
  # 2). Plot WS DC Risk  ----
  
  
  
  ## WS1 ----
  WS1_DC_10 <- zero_centred_barchart(highs      = WS1_highs_10,
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
  
  
  WS1_DC_30 <- zero_centred_barchart(highs      = WS1_highs_30,
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
  
  
  WS1_DC_50 <- zero_centred_barchart(highs      = WS1_highs_50,
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
  WS2_DC_10 <- zero_centred_barchart(highs      = WS2_highs_10,
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
  
  
  WS2_DC_30 <- zero_centred_barchart(highs      = WS2_highs_30,
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
  
  
  WS2_DC_50 <- zero_centred_barchart(highs      = WS2_highs_50,
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
#######################################  ---- TBC ---- #####################################
############################################################################################