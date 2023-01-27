
################################  ----- MAPPING FUNCTIONS FOR SDM ANALYSIS ---- #########################################
#########################################################################################################################


## Helper functions

copy.to.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

nan_df <- function(x) {
  do.call(cbind, lapply(x, is.nan))}


#' @title Complete data frame
#' @description Remove taxa records with NA enviro (i.e. Raster) values - records outside the exetent of rasters
#' @param data         Data.frame of taxa records
#' @param desiredCols  Character string of columns to search for NA values EG c('rainfall', 'temp'), etc.
#' @return             A df with NA records removed
#' @export completeFun
completeFun <- function(data, desiredCols) {
  
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
  
}



flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}



# correlationn_df = HARC_flow_measures_10
# respose         = 'pc50th'
# vars            = enviro_vars
# lim = 'lim10_'

## 
pairwise_correlations <- function(correlationn_df,
                                  respose,
                                  lim,
                                  vars) {
  
  ## Could change the kind of correlation
  vars       <- c(respose, vars)
  var_corr   <- correlationn_df %>% .[ , names(.) %in% vars]
  var_matrix <- rcorr(as.matrix(var_corr), type = "spearman") 
  
  ## Get the negative correlations
  flow_var_correlations_negative <- 
    
    flattenCorrMatrix(var_matrix$r, 
                      var_matrix$P) %>% 
    as_tibble()      %>% 
    filter(cor < 0 ) %>%
    arrange(cor)     %>% filter(row == respose) %>% 
    head() %>% 
    mutate(row = paste0(lim, row))
  
  
  ## Get the positive correlations
  flow_var_correlations_positive <- 
    
    flattenCorrMatrix(var_matrix$r, 
                      var_matrix$P) %>% 
    as_tibble()     %>% 
    filter(cor > 0) %>%
    head()          %>% 
    arrange(-cor)   %>% filter(row == respose) %>% 
    head() %>% 
    mutate(row = paste0(lim, row))
  
  
  ## Take the top two negative and postive
  pos_cor <- flow_var_correlations_positive$column[1:3]
  neg_cor <- flow_var_correlations_negative$column[1:3]
  cor_var <- c(pos_cor, neg_cor)
  
  corr_results <- list(flow_var_correlations_positive, 
                       flow_var_correlations_negative, 
                       cor_var)
  
  names(corr_results) <- c(paste0(respose, '_pos_corr'), 
                           paste0(respose, '_neg_corr'),
                           paste0(respose, '_corr_vars'))
  
  return(corr_results)
  
}


ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}


gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}


#faster replacement for st_intersection(x, y,...)
st_intersection_faster <- function(x,y,...){

  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  
  st_intersection(x, y_subset,...)
}





#' A function for merging sliver polygons into non-sliver neighbours.
#' https://github.com/ianmseddy/eddyTools/blob/master/R/deSliver.R
#' The threshold is applied to the area of the multipolygon object, not each
#' individual polygon. Non-sliver polygons keep their original attributesl
#' Inteded to be used when it is important to retain the original extent of an
#' area while removing sliver polygons
#' @keywords sliver polygons intersect
#' @param x A spatialPolygonsDataFrame or sf object
#' @param threshold the minimum area below which a polygon is considered a sliver
#' @return an object of class sf or Spatial with sliver polygons
#' merged to their nearest valid neighbour.
#'
#' @export
#' @importFrom sf st_area st_cast st_nearest_feature as_Spatial st_union st_as_sf
#' @importFrom raster bind
#' @importFrom rgeos gBuffer
#' @examples
#'deSliver(x = intersectedPolygons, threshold = 500)

deSliver <- function(x, threshold) {
  backToSf <- FALSE
  if (class(x)[1] == "sf") {
    backToSf <- TRUE
    x$tempArea <- as.numeric(st_area(x))
  } else {
    x<- st_as_sf(x)
    x$tempArea <- as.numeric(st_area(x))
  }
  
  #determine slivers by area
  xSlivers <- x[x$tempArea < threshold, ]
  xNotSlivers <- x[x$tempArea > threshold, ]
  if (nrow(xNotSlivers) < 1) {
    stop("Threshold exceeds the area of every polygon. Please select a smaller number")
  }
  
  #Split slivers from multipolygon, or nearest feature may be incorrect
  xSlivers <- suppressWarnings(st_cast(xSlivers, 'POLYGON'))
  
  #Find nearest non-sliver
  nearestFeature <- st_nearest_feature(xSlivers, xNotSlivers)
  
  #Merge each sliver polygon into nearest neighbour
  mergeSlivers <- lapply(
    unique(nearestFeature),
    FUN = function(i,
                   ns = xNotSlivers,
                   s = xSlivers,
                   nf = nearestFeature) {
      featurePolys <- nearestFeature == i
      xMerge <- s[featurePolys, ] %>%
        st_union(.)
      yMerge <- ns[i, ]
      #convert slivers back to multipolygon
      out <- sf::st_union(x = xMerge, y = yMerge) %>%
        as_Spatial(.)
      yMergeSpd <- sf::as_Spatial(yMerge)
      out <- SpatialPolygonsDataFrame(Sr = out,
                                      data = yMergeSpd@data,
                                      match.ID = FALSE)
      
      return(out)
    }
  )
  
  if (length(mergeSlivers) > 1) {
    m <- bind(mergeSlivers)
  } else {
    m <- mergeSlivers[[1]]
  }
  
  #Remove the temporary column
  m$tempArea <- NULL
  
  #remove self-intersecting geometries
  m <- gBuffer(spgeom = m, byid = TRUE, with = 0)
  
  if (backToSf) {
    m <- st_as_sf(m)
  }
  return(m)
}





## BOXPLOT COMBO PLOTS ==============================================================================


# source_list       = harc_list
# model_run        = '_NoDams_100_30_2'
# column           = 'Q_NoDams_100_30_2'
# length_threshold = 1000
# high_quart       = 0.9
# low_quart        = 0.1


## 
hydrostats_list <- function(source_list,
                            column,
                            model_run,
                            length_threshold, 
                            high_quart,
                            low_quart) {
  
  annual_hydro_stats_list <- source_;list %>%
    
    ## Pipe the list into lapply
    #source = source_;list[3]
    lapply(function(source) {
      
      message('calculate flow metrics for ', source)
      NoDams = get(source)
      
      source_dam_time_series <- NoDams %>% 
        dplyr::select(Date, column) %>% 
        na.omit() %>% rename(Q = column) %>% 
        mutate(Date = format(as.POSIXct(Date, 
                                        format = '%d/%m/%Y %H:%M:%S'), 
                             format = '%d/%m/%Y'))
      
      
      ## Find out why this doesn't work - should be 41 years
      source_dam_time_series_baseflows <- baseflows(source_dam_time_series, 
                                                    a  = 0.975, 
                                                    ts = "mean")
      
      source_dam_time_series_high_spell <- high.spell.lengths(source_dam_time_series, 
                                                              quant         = high_quart, 
                                                              ind.days      = 5, 
                                                              ignore.zeros  = TRUE, 
                                                              ctf.threshold = 0.1, 
                                                              inter.flood   = TRUE,
                                                              threshold     = length_threshold)
      
      source_dam_time_series_low_spell  <- low.spell.lengths(source_dam_time_series,
                                                             quant         = low_quart, 
                                                             ind.days      = 5, 
                                                             ignore.zeros  = TRUE, 
                                                             ctf.threshold = 0.1, 
                                                             inter.spell   = TRUE,
                                                             threshold     = length_threshold)
      
      source_dam_time_series_flood_max  <- flood.length.max(source_dam_time_series, 
                                                            threshold = length_threshold, 
                                                            ind.days  = 5)
      
      source_dam_time_series_daily_cv    <- daily.cv(source_dam_time_series)
      source_dam_time_series_monthly_cv  <- monthly.cv(source_dam_time_series)
      source_dam_time_series_seasonality <- seasonality(source_dam_time_series, monthly.range = TRUE)
      
      
      source_dam_time_series_season_df   <- source_dam_time_series_seasonality$seasonality %>% 
        as.data.frame()
      names(source_dam_time_series_season_df) <- c('seasonality')
      
      source_dam_time_series_numeric     <- as_tibble(c(source_dam_time_series_daily_cv,
                                                        source_dam_time_series_monthly_cv,
                                                        source_dam_time_series_season_df))
      
      flow_stats_results <- list(source_dam_time_series_baseflows, 
                                 source_dam_time_series_flood_max,
                                 source_dam_time_series_high_spell,
                                 source_dam_time_series_low_spell,
                                 source_dam_time_series_numeric)
      
      names(flow_stats_results) <- c(paste0(source, model_run, '_baseflows'), 
                                     paste0(source, model_run, '_flood_max'),
                                     paste0(source, model_run, '_high_spell'),
                                     paste0(source, model_run, '_low_spell'),
                                     paste0(source, model_run, '_variation'))
      
      return(flow_stats_results)
      
    })
  
  names(annual_hydro_stats_list) <- source_list
  names(annual_hydro_stats_list) <- paste0(names(annual_hydro_stats_list), model_run)
  return(annual_hydro_stats_list)
}




# 
# column_list      = names(baseline_ws_gauges_combined)
# model_run        = '_WithDams_100_50_2'
# column           = 'Q_WithDams_100_50_2'
# compare_colum    = 'Q_NoDams_100_50_2'
# length_threshold = 1000
# high_quart       = 0.9
# low_quart        = 0.1


hydrostats_gague_table <- function(base_list,
                                   column,
                                   compare_colum,
                                   model_run,
                                   length_threshold, 
                                   high_quart,
                                   low_quart) {
  
  hydro_flow_stats_df <- source_list %>%
    
    ## Pipe the list into lapply
    #source = source_list[1]
    lapply(function(source) {
      
      message('calculate flow metrics for ', source)
      station = get(source)
      
      station_time_series <- station %>% 
        dplyr::select(Date, "BEGA_219001_Rutherford Brown Mtn") %>% 
        na.omit() %>%  
        
        mutate(Date = format(as.POSIXct(Date, 
                                        format = '%d/%m/%Y %H:%M:%S'), 
                             format = '%d/%m/%Y'))
      
      # source_name <- station$Water_source %>% unique()
      
      source_nodam_time_series <- station %>% 
        dplyr::select(Date, compare_colum) %>% 
        na.omit() %>% rename(Q = compare_colum) %>% 
        
        mutate(Date = format(as.POSIXct(Date, 
                                        format = '%d/%m/%Y %H:%M:%S'), 
                             format = '%d/%m/%Y'))
      
      ## Replicate this for No station
      ## Find out why this doesn't work - should be 41 years, not 31!!!
      station_time_series_baseflows <- baseflows(station_time_series, 
                                                 a  = 0.975, 
                                                 ts = "mean")
      
      station_time_series_baseflows_mn <- station_time_series_baseflows %>%
        mutate(Water_source = source_name,
               Dam_Model = column) %>% 
        dplyr::select(Water_source, Dam_Model, mean.bf) 
      
      station_time_series_high_spell <- high.spell.lengths(station_time_series, 
                                                           quant         = high_quart, 
                                                           # threshold     = length_threshold,
                                                           ind.days      = 5, 
                                                           ignore.zeros  = TRUE, 
                                                           ctf.threshold = 0.1, 
                                                           inter.flood   = TRUE)
      
      station_time_series_high_spell_mn <- mean(station_time_series_high_spell$spell.length, 
                                                na.rm = TRUE) %>% as.data.frame()
      
      names(station_time_series_high_spell_mn)       <- c('low_spell')
      station_time_series_high_spell_mn$Water_source <- source_name
      station_time_series_high_spell_mn$Dam_Model    <- column
      
      station_time_series_low_spell <- low.spell.lengths(station_time_series,
                                                         quant         = low_quart, 
                                                         ind.days      = 5, 
                                                         ignore.zeros  = TRUE, 
                                                         ctf.threshold = 0.1, 
                                                         inter.spell   = TRUE,
                                                         threshold     = length_threshold)
      
      station_time_series_low_spell_mn <- mean(station_time_series_low_spell$spell.length, 
                                               na.rm = TRUE) %>% as.data.frame()
      
      names(station_time_series_low_spell_mn)      <- c('high_spell')
      station_time_series_low_spell_mn$Water_source   <- source_name
      station_time_series_low_spell_mn$Dam_Model   <- column
      
      station_time_series_flood_max  <- flood.length.max(station_time_series, 
                                                         threshold = length_threshold, 
                                                         ind.days  = 5)
      
      station_time_series_flood_max$Water_source   <- source_name
      station_time_series_flood_max$Dam_Model <- column
      
      station_time_series_daily_cv    <- daily.cv(station_time_series)
      station_time_series_monthly_cv  <- monthly.cv(station_time_series)
      station_time_series_seasonality <- seasonality(station_time_series, monthly.range = TRUE)
      
      
      station_time_series_season_df   <- station_time_series_seasonality$seasonality %>% 
        as.data.frame()
      names(station_time_series_season_df) <- c('seasonality')
      
      station_time_series_variaton     <- as_tibble(c(station_time_series_daily_cv,
                                                      station_time_series_monthly_cv,
                                                      station_time_series_season_df)) %>% 
        mutate(Water_source  = source_name,
               Dam_Model = column) %>% dplyr::select(Water_source, Dam_Model, everything())
      
      ## Replicate this for No station
      ## Find out why this doesn't work - should be 41 years, not 31!!!
      source_nodam_time_series_baseflows <- baseflows(source_nodam_time_series, 
                                                      a  = 0.975, 
                                                      ts = "mean")
      
      source_nodam_time_series_baseflows_mn <- source_nodam_time_series_baseflows %>%
        mutate(Water_source = source_name,
               Dam_Model = column) %>% 
        dplyr::select(Water_source, Dam_Model, mean.bf) 
      
      source_nodam_time_series_high_spell <- high.spell.lengths(source_nodam_time_series, 
                                                                quant         = high_quart, 
                                                                # threshold     = length_threshold,
                                                                ind.days      = 5, 
                                                                ignore.zeros  = TRUE, 
                                                                ctf.threshold = 0.1, 
                                                                inter.flood   = TRUE)
      
      source_nodam_time_series_high_spell_mn <- mean(source_nodam_time_series_high_spell$spell.length, 
                                                     na.rm=TRUE) %>% as.data.frame()
      
      names(source_nodam_time_series_high_spell_mn)    <- c('low_spell')
      source_nodam_time_series_high_spell_mn$Water_source <- source_name
      source_nodam_time_series_high_spell_mn$Dam_Model <- column
      
      source_nodam_time_series_low_spell <- low.spell.lengths(source_nodam_time_series,
                                                              quant         = low_quart, 
                                                              ind.days      = 5, 
                                                              ignore.zeros  = TRUE, 
                                                              ctf.threshold = 0.1, 
                                                              inter.spell   = TRUE,
                                                              threshold     = length_threshold)
      
      source_nodam_time_series_low_spell_mn <- mean(source_nodam_time_series_low_spell$spell.length, 
                                                    na.rm = TRUE) %>% as.data.frame()
      
      names(source_nodam_time_series_low_spell_mn)      <- c('high_spell')
      source_nodam_time_series_low_spell_mn$Water_source   <- source_name
      source_nodam_time_series_low_spell_mn$Dam_Model   <- column
      
      source_nodam_time_series_flood_max  <- flood.length.max(source_nodam_time_series, 
                                                              threshold = length_threshold, 
                                                              ind.days  = 5)
      
      source_nodam_time_series_flood_max$Water_source   <- source_name
      source_nodam_time_series_flood_max$Dam_Model <- column
      
      source_nodam_time_series_daily_cv    <- daily.cv(source_nodam_time_series)
      source_nodam_time_series_monthly_cv  <- monthly.cv(source_nodam_time_series)
      source_nodam_time_series_seasonality <- seasonality(source_nodam_time_series, monthly.range = TRUE)
      
      
      source_nodam_time_series_season_df   <- source_nodam_time_series_seasonality$seasonality %>% 
        as.data.frame()
      names(source_nodam_time_series_season_df) <- c('seasonality')
      
      source_nodam_time_series_variaton     <- as_tibble(c(source_nodam_time_series_daily_cv,
                                                           source_nodam_time_series_monthly_cv,
                                                           source_nodam_time_series_season_df)) %>% 
        mutate(Water_source  = source_name,
               Dam_Model = column) %>% dplyr::select(Water_source, Dam_Model, everything())
      
      
      ## Combine them together
      station_time_series_all <- 
        
        ## rename later, x = dam, y = no dam
        list(station_time_series_variaton,
             source_nodam_time_series_variaton,
             
             station_time_series_baseflows_mn,
             source_nodam_time_series_baseflows_mn,
             
             station_time_series_high_spell_mn,
             source_nodam_time_series_high_spell_mn,
             
             station_time_series_low_spell_mn,
             source_nodam_time_series_low_spell_mn,
             
             station_time_series_flood_max,
             source_nodam_time_series_flood_max) %>%
        
        reduce(left_join, c("Water_source", "Dam_Model"))
      
      return(station_time_series_all)
      
    }) %>% bind_rows() 
  
}




# 
# source_list      = harc_list
# model_run        = '_WithDams_100_50_2'
# source_col       = 'CHR_Water_Source'
# column           = 'Q_WithDams_100_50_2'
# compare_colum    = 'Q_NoDams_100_50_2'
# 
# length_threshold = 1000 ## Flow threshold, change this
# high_quart       = 0.9
# low_quart        = 0.1


hydrostats_dam_table <- function(source_list,
                                 source_col,
                                 column,
                                 compare_colum,
                                 model_run,
                                 length_threshold, 
                                 high_quart,
                                 low_quart) {
  
  hydro_flow_stats_df <- source_list %>%
    
    ## Pipe the list into lapply
    #source = source_list[1]
    lapply(function(source) {
      
      message('calculate flow metrics for ', source)
      Dams = get(source)
      
      source_dam_time_series <- Dams %>% 
        dplyr::select(Date, column)  %>% 
        na.omit() %>% rename(Q = column) %>% 
        
        mutate(Date = format(as.POSIXct(Date, 
                                        format = '%d/%m/%Y %H:%M:%S'), 
                             format = '%d/%m/%Y'))
      
      source_name <- Dams[[source_col]] %>% unique()
      
      source_nodam_time_series <- Dams %>% 
        dplyr::select(Date, compare_colum) %>% 
        na.omit() %>% rename(Q = compare_colum) %>% 
        
        mutate(Date = format(as.POSIXct(Date, 
                                        format = '%d/%m/%Y %H:%M:%S'), 
                             format = '%d/%m/%Y'))
      
      ## Replicate this for No dams
      ## Find out why this doesn't work - should be 41 years, not 31!!!
      source_dam_time_series_baseflows <- baseflows(source_dam_time_series, 
                                                    a  = 0.975, 
                                                    ts = "mean")
      
      source_dam_time_series_baseflows_mn <- source_dam_time_series_baseflows %>%
        
        mutate(!!source_col := source_name,
               Dam_Model        = column) %>% 
        dplyr::select(!!as.symbol(source_col), Dam_Model, mean.bf) 
      
      source_dam_time_series_high_spell <- high.spell.lengths(source_dam_time_series, 
                                                              quant         = high_quart, 
                                                              # threshold     = length_threshold,
                                                              ind.days      = 5, 
                                                              ignore.zeros  = TRUE, 
                                                              ctf.threshold = 0.1, 
                                                              inter.flood   = TRUE)
      
      source_dam_time_series_high_spell_mn <- mean(source_dam_time_series_high_spell$spell.length, 
                                                   na.rm = TRUE) %>% 
        as.data.frame()
      
      names(source_dam_time_series_high_spell_mn)        <- c('low_spell')
      source_dam_time_series_high_spell_mn[[source_col]] <- source_name
      source_dam_time_series_high_spell_mn$Dam_Model     <- column
      
      source_dam_time_series_low_spell <- low.spell.lengths(source_dam_time_series,
                                                            quant         = low_quart, 
                                                            ind.days      = 5, 
                                                            ignore.zeros  = TRUE, 
                                                            ctf.threshold = 0.1, 
                                                            inter.spell   = TRUE,
                                                            threshold     = length_threshold)
      
      source_dam_time_series_low_spell_mn <- mean(source_dam_time_series_low_spell$spell.length, 
                                                  na.rm = TRUE) %>% as.data.frame()
      
      names(source_dam_time_series_low_spell_mn)        <- c('high_spell')
      source_dam_time_series_low_spell_mn[[source_col]] <- source_name
      source_dam_time_series_low_spell_mn$Dam_Model     <- column
      
      source_dam_time_series_flood_max  <- flood.length.max(source_dam_time_series, 
                                                            threshold = length_threshold, 
                                                            ind.days  = 5)
      
      source_dam_time_series_flood_max[[source_col]]   <- source_name
      source_dam_time_series_flood_max$Dam_Model       <- column
      
      source_dam_time_series_daily_cv    <- daily.cv(source_dam_time_series)
      source_dam_time_series_monthly_cv  <- monthly.cv(source_dam_time_series)
      source_dam_time_series_seasonality <- seasonality(source_dam_time_series, monthly.range = TRUE)
      
      
      source_dam_time_series_season_df   <- source_dam_time_series_seasonality$seasonality %>% 
        as.data.frame()
      names(source_dam_time_series_season_df) <- c('seasonality')
      
      source_dam_time_series_variaton     <- as_tibble(c(source_dam_time_series_daily_cv,
                                                         source_dam_time_series_monthly_cv,
                                                         source_dam_time_series_season_df)) %>% 
        mutate(!!source_col := source_name,
               Dam_Model     = column) %>% dplyr::select(!!as.symbol(source_col), Dam_Model, everything())
      
      
      
      ## Replicate this for No dams
      ## Find out why this doesn't work - should be 41 years, not 31!!!
      source_nodam_time_series_baseflows <- baseflows(source_nodam_time_series, 
                                                      a  = 0.975, 
                                                      ts = "mean")
      
      source_nodam_time_series_baseflows_mn <- source_nodam_time_series_baseflows %>%
        mutate(!!source_col := source_name,
               Dam_Model     = column) %>% 
        dplyr::select(!!as.symbol(source_col), Dam_Model, mean.bf) 
      
      source_nodam_time_series_high_spell <- high.spell.lengths(source_nodam_time_series, 
                                                                quant         = high_quart, 
                                                                # threshold     = length_threshold,
                                                                ind.days      = 5, 
                                                                ignore.zeros  = TRUE, 
                                                                ctf.threshold = 0.1, 
                                                                inter.flood   = TRUE)
      
      source_nodam_time_series_high_spell_mn <- mean(source_nodam_time_series_high_spell$spell.length, 
                                                     na.rm=TRUE) %>% as.data.frame()
      
      names(source_nodam_time_series_high_spell_mn)    <- c('low_spell')
      source_nodam_time_series_high_spell_mn[[source_col]] <- source_name
      source_nodam_time_series_high_spell_mn$Dam_Model <- column
      
      source_nodam_time_series_low_spell <- low.spell.lengths(source_nodam_time_series,
                                                              quant         = low_quart, 
                                                              ind.days      = 5, 
                                                              ignore.zeros  = TRUE, 
                                                              ctf.threshold = 0.1, 
                                                              inter.spell   = TRUE,
                                                              threshold     = length_threshold)
      
      source_nodam_time_series_low_spell_mn <- mean(source_nodam_time_series_low_spell$spell.length, 
                                                    na.rm = TRUE) %>% as.data.frame()
      
      names(source_nodam_time_series_low_spell_mn)       <- c('high_spell')
      source_nodam_time_series_low_spell_mn[[source_col]] <- source_name
      source_nodam_time_series_low_spell_mn$Dam_Model    <- column
      
      source_nodam_time_series_flood_max  <- flood.length.max(source_nodam_time_series, 
                                                              threshold = length_threshold, 
                                                              ind.days  = 5)
      
      source_nodam_time_series_flood_max[[source_col]] <- source_name
      source_nodam_time_series_flood_max$Dam_Model    <- column
      
      source_nodam_time_series_daily_cv    <- daily.cv(source_nodam_time_series)
      source_nodam_time_series_monthly_cv  <- monthly.cv(source_nodam_time_series)
      source_nodam_time_series_seasonality <- seasonality(source_nodam_time_series, monthly.range = TRUE)
      
      
      source_nodam_time_series_season_df   <- source_nodam_time_series_seasonality$seasonality %>% 
        as.data.frame()
      names(source_nodam_time_series_season_df) <- c('seasonality')
      
      source_nodam_time_series_variaton         <- as_tibble(c(source_nodam_time_series_daily_cv,
                                                               source_nodam_time_series_monthly_cv,
                                                               source_nodam_time_series_season_df)) %>% 
        mutate(!!source_col := source_name,
               Dam_Model     = column) %>% 
        
        dplyr::select(!!as.symbol(source_col), 
                      Dam_Model, everything())
      
      ## Combine them together
      source_dam_time_series_all <- 
        
        ## rename later, x = dam, y = no dam
        list(source_dam_time_series_variaton,
             source_nodam_time_series_variaton,
             
             source_dam_time_series_baseflows_mn,
             source_nodam_time_series_baseflows_mn,
             
             source_dam_time_series_high_spell_mn,
             source_nodam_time_series_high_spell_mn,
             
             source_dam_time_series_low_spell_mn,
             source_nodam_time_series_low_spell_mn,
             
             source_dam_time_series_flood_max,
             source_nodam_time_series_flood_max) %>%
        
        reduce(left_join, c(source_col, "Dam_Model"))
      
      return(source_dam_time_series_all)
      
    }) %>% bind_rows() 
  
}





## Create a list of overall boxplots for each group ----
line_plot_list <- function(plot_list, data,  
                           
                           ## This creates the question number
                           x_var, y_var, v_just, mar, pallette, group_var,
                           box_size,  x_lab,     y_lab, y_lim, leg_pos,
                           lab_size,  lab_angle, border_size) {
  
  ## Pipe the list into Lapply
  line_plot_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## source <- plot_list[1]
    lapply(function(source) {
      
      ## Check the dimensions of the data
      message('creating line plots for ', source)
      source_data  <- data %>% filter(Water_source == source) %>% 
        
        ## Create ggplot
        ggplot(aes(Limit_percent, Mean_annual_flow_perc_change, color = Water_source)) +
        geom_line(size = 2, alpha = .8) +
        theme_light(base_size = 16) +
        # scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
        
        ##
        labs(title = paste("Limit % vs. Mean annual flow change% @ current uptake " , 
                           format(Sys.Date(), format = "%d-%b-%y"),")", "\n", sep = ""),
             x = 'Limit (%)',
             y = "Mean annual Flow change (%)\n\n") +  
        
        ## Create the themes
        theme_light(base_size = 16) +
        
        theme(axis.title.x = element_text(face   = "bold", colour = "black", size = 15),
              axis.text.x  = element_text(size   = 15, angle = 0, hjust = 1, vjust = 0.5),
              axis.text.y  = element_text(size   = 15),
              axis.title.y = element_text(face   = "bold", 
                                          colour = "black", 
                                          size   = 15),
              legend.position  = 'right',
              legend.title     = element_blank(),
              
              panel.grid.major = element_line(size   = 0.5, linetype = 'solid',
                                              colour = "grey"), 
              panel.grid.minor = element_line(size   = 0.5, linetype = 'solid',
                                              colour = "grey"))
      
    }) %>% c() 
  
  ## Rename the list items
  names(line_plot_list) <- plot_list
  names(line_plot_list) <- paste0(names(line_plot_list), "_line_plot")
  return(line_plot_list)
  
}





## Create a list of overall boxplots for each group ----
boxplot_list <- function(plot_list, survey_data, columns, 
                         role_list, count_list, agg_level,
                         
                         ## This creates the question number
                         x_var, y_var, v_just, mar, pallette, group_var,
                         box_size,  x_lab,     y_lab, y_lim, leg_pos,
                         lab_size,  lab_angle, border_size) {
  
  ## Pipe the list into Lapply
  boxplot_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      y_lab = y_lab
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_var)
      bp_role  <- respondent_index %>% dplyr::select(one_of(cols)) %>%
        
        gather(key = "Category", value = "Index", -!!as.symbol(group_var)) %>%
        dplyr::filter(Index != 0) %>% 
        mutate(!!group_var := as.factor(!!as.symbol(group_var))) %>%
        mutate(Category = factor(Category, levels = index_plot_columns),
               Index    = as.numeric(Index)) %>% 
        completeFun(., 'Category') %>% na.omit() %>% 
        
        factor_boxplots(., 
                        x_var       = x_var, 
                        y_var       = y_var, 
                        group_var   = group_var,
                        leg_pos     = leg_pos,
                        y_lab       = y_lab, 
                        x_lab       = x_lab,
                        pallette    = pallette,
                        v_just      = v_just,
                        mar         = mar,
                        
                        box_size    = box_size, 
                        y_lim       = y_lim, 
                        lab_angle   = lab_angle,
                        lab_size    = lab_size,
                        border_size = border_size)
      
      
    }) %>% c() 
  
  ## Rename the list items
  names(boxplot_list) <- plot_list
  names(boxplot_list) <- paste0(names(boxplot_list), "_boxplot")
  return(boxplot_list)
  
}





## Create a list of grouped boxplots for each topic ----
boxplot_group_list <- function(plot_list, survey_data, agg_level,
                               columns, levels, v_just, mar,
                               
                               ## This creates the question number
                               x_var, y_var,         pallette, group_vars,
                               group_var,
                               box_size,  x_lab,     y_lab, y_lim, leg_pos,
                               lab_size,  lab_angle, border_size) {
  
  ## Pipe the list into Lapply
  boxplot_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      # x_lab = topic
      y_lab = y_lab
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_vars)
      bp_role  <- survey_data %>% dplyr::select(one_of(cols)) %>%
        
        pivot_longer(cols = group_vars, values_to = "Category") %>%
        rename(Index   = !!as.symbol(topic)) %>% 
        dplyr::filter(Index  != 0) %>% 
        mutate(Category = factor(Category, levels = unique(levels))) %>% 
        completeFun(., 'Category') %>% na.omit() %>% 
        
        factor_boxplots(., 
                        x_var       = x_var, 
                        y_var       = y_var, 
                        group_var   = group_var,
                        leg_pos     = leg_pos,
                        y_lab       = y_lab, 
                        x_lab       = x_lab,
                        pallette    = pallette,
                        v_just      = v_just,
                        mar         = mar,
                        
                        box_size    = box_size, 
                        y_lim       = y_lim, 
                        lab_angle   = lab_angle,
                        lab_size    = lab_size,
                        border_size = border_size)      
      
    }) %>% c() 
  
  ## Rename the list items
  names(boxplot_list) <- plot_list
  names(boxplot_list) <- paste0(names(boxplot_list), "_boxplot")
  return(boxplot_list)
  
}





## SCATTER COMBO PLOTS ==============================================================================


# scatplot_list      = names(respondent_load_scatterplots)[-1] 
# 
# scat_var           = "Venue_Voting_Load"
# index_plot_columns = index_plot_columns
# x_label            = 'Venue Workload (Votes / Projections)'
# 
# axis_size   = 10 
# axis_title  = 25 
# point_col   = "blue"
# point_size  = 3
# labelSize   = 25
# title_size  = 30 
# leg_size    = 20 
# legend_pos  = 'none'

## Names
# scatplot_names = names(respondent_load_scatterplots)[-1]



## Key variable scatterplot list witout grouping variable ---- 
scatter_keyvar_ungroup_list = function(scatplot_list, 
                                       scatplot_names, 
                                       group_var, 
                                       scat_var,
                                       x_label,
                                       index_plot_columns,
                                       axis_size, 
                                       axis_title, 
                                       point_col,
                                       point_size,
                                       labelSize,
                                       title_size, 
                                       leg_size, 
                                       legend_pos) {
  
  ## Pipe the list into lapply
  scatterplot_indexes <- scatplot_list %>%
    
    lapply(function(index) {
      
      #index = scatplot_list[1]
      # label      <- names(index)
      scatter_df <- get(index)
      # title      <- gsub("\\_index_.*", " Workload vs. Index", index) %>% firstup()
      level      <- gsub("\\_index_.*", "", index) %>% firstup()
      
      ## Create table
      message("Creating survey index scatterplots for ", names(index), ' ', nrow(scatter_df), ' rows')
      scatter_plot <- scatter_matrix_keyvar(scat_df <- scatter_df %>% 
                                              dplyr::select(one_of(scat_var, index_plot_columns)) %>% 
                                              melt(., scat_var),
                                            
                                            ## This will chop off the last column
                                            scat_var    = scat_var,
                                            axis_size   = axis_size, 
                                            axis_title  = axis_title, 
                                            point_col   = point_col,
                                            point_size  = point_size,
                                            labelSize   = labelSize,
                                            title_size  = title_size, 
                                            leg_size    = leg_size , 
                                            legend_pos  = legend_pos,
                                            ylab        = 'Index Value',
                                            # xlab        = paste0(level, x_label),
                                            xlab        =  x_label,
                                            title       = '')
      
    }) %>% c() 
  
  ## Rename the list items
  names(scatterplot_indexes) <- scatplot_names
  return(scatterplot_indexes)
  
}





## Scatterplot matrix list without grouping variable ---- 
scatter_matrix_ungroup_list = function(scatplot_list, scatplot_names, group_var) {
  
  ## Pipe the list into lapply
  scatterplot_indexes <- scatplot_list %>%
    
    lapply(function(index) {
      
      #index = plot_index_names[3]
      scatter_df <- get(index) %>% dplyr::select(-group_var)
      cols_nums  <- 1:ncol(scatter_df)
      
      ## Create table
      message("Creating survey index scatterplots for ", names(index), ' ', nrow(scatter_df), ' rows')
      scatter_plot <- scatter_matrix_colour(scat_df     = scatter_df,
                                            
                                            ## This will chop off the last column
                                            cols        = cols_nums,
                                            scat_col    = "turquoise4",
                                            
                                            alpha       = 0.5, 
                                            alignPer    = 0.8,
                                            upper_size  = 10, 
                                            lower_size  = 3, 
                                            axis_size   = 20, 
                                            title_size  = 30, 
                                            leg_size    = 30, 
                                            legend_pos  = 'none',
                                            title       = '') 
      
    }) %>% c() 
  
  ## Rename the list items
  names(scatterplot_indexes) <- scatplot_names
  return(scatterplot_indexes)
  
}





## Scatterplot matrix list with grouping variable ---- 
scatter_matrix_group_list = function(scatplot_list, scatplot_names) {
  
  ## Pipe the list into lapply
  scatterplot_indexes <- scatplot_list %>%
    
    lapply(function(index) {
      
      #index = plot_index_names[3]
      scatter_df <- get(index)
      cols_nums  <- 1:ncol(scatter_df)
      
      ## Create table
      message("Creating survey index scatterplots for ", names(index), ' ', nrow(scatter_df), ' rows')
      scatter_plot <- scatter_matrix_colour(scat_df     = scatter_df,
                                            
                                            ## This will chop off the last column
                                            cols        = cols_nums,
                                            scat_col    = "turquoise4",
                                            
                                            alpha       = 0.5, 
                                            alignPer    = 0.8,
                                            upper_size  = 10, 
                                            lower_size  = 3, 
                                            axis_size   = 20, 
                                            title_size  = 30, 
                                            leg_size    = 30, 
                                            legend_pos  = 'none',
                                            title       = '') 
      
    }) %>% c() 
  
  ## Rename the list items
  names(scatterplot_indexes) <- scatplot_names
  return(scatterplot_indexes)
  
}



## STAT FUNCTIONS ==============================================================================



## Create a list of grouped boxplots for each subset ----
anova_group_list <- function(plot_list, 
                             data, 
                             columns, 
                             group_var,
                             group_vars,
                             levels) {
  
  ## Pipe the list into Lapply
  anova_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      ## Check the dimensions of the data
      message('running anova for demographics of', topic)
      cols     <- c(topic, group_vars)
      anova_demog <- data %>% dplyr::select(one_of(cols)) %>%
        
        pivot_longer(cols = group_vars, values_to = "Category") %>%
        rename(Index   = !!as.symbol(topic)) %>% 
        dplyr::filter(Index  != 0) %>% 
        dplyr::mutate(Category = factor(Category, levels = unique(levels))) %>% 
        completeFun(., 'Category') %>% na.omit()  %>% 
        
        ## 
        aov(Index ~ Category,  data = .)
      
      resid_plot <- data_frame(
        fitted  = predict(anova_demog),
        residual = residuals(anova_demog)) %>%
        
        ## and then plot points and a smoothed line
        ggplot(aes(fitted, residual)) +
        geom_point() +
        geom_hline(yintercept = 0, col = "red")
      
      tukey_test  <- TukeyHSD(anova_demog) %>% .[[1]]
      tukey_table <- tukey_test %>% as_tibble()
      tukey_table$compare = rownames(tukey_test)
      tukey_table <- dplyr::select(tukey_table, compare, everything())
      
      anova_results <- list(anova_demog, resid_plot, tukey_table)
      names(anova_results) <- c(paste0(topic, '_anova_role'), 
                                paste0(topic, '_resid_plot'),
                                paste0(topic, '_tukey_test'))
      
      return(anova_results)
      
    })
  
  ## Rename the list items
  names(anova_list) <- plot_list
  names(anova_list) <- paste0(names(anova_list), "_anova")
  return(anova_list)
  
}





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################
