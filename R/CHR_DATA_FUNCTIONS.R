
################################  ----- MAPPING FUNCTIONS FOR SDM ANALYSIS ---- #########################################
#########################################################################################################################


## Below are the functions used to project SDM models across geographic areas.

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





#' Resampling a Raster* object via rGDAL.
#' 
#' @param rast Raster* object to be resampled
#' @param rast_base Raster* object with parameters that r
#' should be resampled to.
#' @param method Character. GDAL resampling_method
#' ("near"|"bilinear"|"cubic"|"cubicspline"|
#'  "lanczos"|"average"|"mode"|"max"|"min"|
#'  "med"|"q1"|"q3")
#' @param temp_dir Character. 
#' @param write_rasters Character. 
#' @param output_path Character. 
#' @param output_file Character. 
#' @export gdal_resample
gdal_resample <- function(rast, 
                          rast_base, 
                          method, 
                          temp_dir,
                          write_rasters,
                          output_path,
                          output_file) {
  
  ## Geometry attributes
  t1 <- c(xmin(rast_base), ymin(rast_base), 
          xmax(rast_base), ymax(rast_base))
  res <- res(rast_base)
  
  ## Temporary files
  tmp_outname <- sprintf('%sout.tif', temp_dir)
  tmp_inname  <- sprintf('%sin.tif',  temp_dir)
  
  message('saving temporary raster')
  writeRaster(rast, 
              tmp_inname, 
              datatype  = "INT2U", 
              options   = "COMPRESS=LZW",
              overwrite = TRUE)
  
  ## GDAL time!
  message('resampling raster using gdalwarp')
  gdalwarp(tmp_inname, tmp_outname, 
           tr = res, te = t1, r = method)
  resample_raster = raster(tmp_outname)
  
  if(write_rasters) {
    
    message('saving resampled raster to file')
    writeRaster(resample_raster, 
                paste0(output_path, output_file),
                datatype  = "INT2U", 
                options   = "COMPRESS=LZW",
                overwrite = TRUE)
    
    return(resample_raster)
    
  } else  {
    return(resample_raster)  
  }
}  





#' @title Threshold current habitat suitability rasters.
#' @description Takes a habitat suitability layer, and creates a binary suitability layer (0, 1) using a threshold value.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param maxent_path        Character string - The file path containing the existing maxent models
#' @param maxent_table       Data frame       - A table of maxent results to be used for mapping 
#' @param cell_factor        Numeric          - Cell size to resample output
#' @param country_shp        Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param country_prj        Character string - Name of projection
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @export habitat_threshold
habitat_threshold = function(taxa_list,
                             maxent_table,
                             maxent_path,
                             poly_path,
                             epsg) {
  
  
  ## Convert to SF object for selection - inefficient
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[3]
    lapply(function(taxa) {
      
      ## Get the directory
      DIR = maxent_table %>%
        filter(searchTaxon == taxa) %>%
        dplyr::select(results_dir) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      thresh = maxent_table %>%
        filter(searchTaxon == taxa) %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      save_name <- gsub(' ', '_', taxa)
      
      ## Check the threshold data exists
      current_file  = sprintf('%s/%s/full/%s_current_not_novel.tif',
                              maxent_path, save_name, save_name)
      
      current_thresh =  sprintf('%s/%s/full/%s_%s%s.tif', maxent_path,
                                save_name, save_name, "current_suit_not_novel_above_", thresh)
      
      ## If the threshold raster data doesn't exist :
      if(file.exists(current_file)) {
        
        if(!file.exists(current_thresh)) {
          
          ## Print the taxa being analysed
          message('doing ', taxa, ' | Logistic > ', thresh)
          
          ## Read in the current suitability raster :: get the current_not_novel raster
          f_current <- raster(sprintf('%s/%s/full/%s_current_not_novel.tif',
                                      maxent_path, save_name, save_name))
          
          ## First, create a simple function to threshold each of the rasters in raster.list,
          ## Then apply this to just the current suitability raster.
          thresh_greater       = function (x) {x > thresh}
          current_suit_thresh  = thresh_greater(f_current)
          current_suit_rast    = terra::rast(current_suit_thresh)
          
          ## Re-sample rasters
          # message('Resampling Model for ', taxa, ' to ', xres(Ref_raster), 'm')
          # current_suit_thresh_resample <- terra::disagg(current_suit_rast, fact = cell_factor)
          
          ## Write the current suitability raster, threshold-ed using the Maximum training
          ## sensitivity plus specificity Logistic threshold
          message('Writing ', taxa, ' current', ' max train > ', thresh)
          
          ## Save in two places, in the taxa folder, 
          ## and in the habitat suitability folder
          writeRaster(current_suit_rast, 
                      sprintf('%s/%s/full/%s_%s%s.tif', maxent_path,
                              save_name, save_name, "current_suit_not_novel_above_", thresh),
                      overwrite = TRUE)
          
          vals       <- terra::unique(current_suit_rast)
          uniue_vals <- is.na(vals[[1]]) %>% unique()
          
          gc()
          
          if(!uniue_vals) {
            
            message('Converting ', taxa, ' raster to repaired polygon')
            
            current_thresh_poly      <- terra::as.polygons(current_suit_rast) 
            current_thresh_poly_dat  <- terra::subset(current_thresh_poly, current_thresh_poly$layer == 1)
            current_thresh_poly_geom <- current_thresh_poly_dat %>% st_as_sf() %>% repair_geometry()
            
            ## Now save the thresh-holded rasters as shapefiles
            message('Saving current threshold SDM rasters to polygons for ', taxa)
            st_write(current_thresh_poly_geom,
                     
                     dsn    = sprintf('%s/%s/full/%s_%s%s.gpkg', 
                                      maxent_path,
                                      save_name, 
                                      save_name, 
                                      'current_suit_not_novel_above_', 
                                      thresh),
                     
                     layer  = paste0(save_name, 
                                     '_current_suit_not_novel_above_', 
                                     thresh),
                     
                     quiet  = TRUE,
                     append = FALSE)
            gc()
            
          } else {
            message('Do not save current MESS maps to shapefile for ', taxa, ' no cells are novel')
          }
          
          
          message('writing threshold png for ', taxa)
          png(sprintf('%s/%s/full/%s_%s%s.png', maxent_path,
                      save_name, save_name, "current_suit_not_novel_above_", thresh),
              16, 10, units = 'in', res = 500)
          
          ##
          raster::plot(current_suit_thresh, main = paste0(taxa, ' > ', thresh))
          raster::plot(poly, add = TRUE)
          dev.off()
          
        } else {
          message('Habitat Suitability threshold raster already exists for', taxa, ' skip')
          cat(taxa)
        }
        
      } else {
        message('No Habitat Suitability raster for', taxa, ' skip')
        cat(taxa)
      }
    }) 
}





#' @title Taxa records intersect 
#' @description Take a table of taxa records, and intersect the records for each taxon with a feature 
#' layer (e.g. Vegetation).

#' @param analysis_df     SpatialPolygonsDataFrame - Spdf of all the taxa analysed
#' @param taxa_list       Character string - The taxa to run maxent predictions for
#' @param taxa_level      Character string - the taxnomic level to run maxent models for
#' @param habitat_raster  Character string - The habitat raster which has already been read in
#' @param country_shp     Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param buffer          Numeric          - Distance by which to buffer the points (metres using a projected system)
#' @param raster_convert  Logical          - Convert to raster?
#' @param save_shp        Logical          - Save as .shp? Geopackage is much better.
#' @param save_png        Logical          - Save as .png?
#' @param poly_path       Character string - file path to feature polygon layer
#' @param int_cols        Character string - list of columns to keep from records * layer intersect
#' @param epsg            Numeric - ERSP code of coord ref system to be translated into WKT format
#' @export taxa_records_habitat_features_intersect
taxa_records_habitat_features_intersect = function(analysis_df,
                                                   taxa_list,
                                                   taxa_level,
                                                   habitat_poly,
                                                   output_path,
                                                   buffer,
                                                   raster_convert,
                                                   int_cols,
                                                   save_shp,
                                                   save_png,
                                                   epsg,
                                                   poly_path) {
  
  ## Loop over each directory
  ## taxa = taxa_list[10]
  sf_use_s2(FALSE)
  
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  lapply(taxa_list, function(taxa) {
    
    ## Check if the taxa exists
    if(taxa %in%  unique(analysis_df$searchTaxon)) {
      
      save_name  <- gsub(' ', '_', taxa)
      veg_inter  <- paste0(output_path, save_name, '_SDM_VEG_intersection.gpkg')
      
      if(!file.exists(veg_inter)) {
        
        ## For each taxon, get the same records that were used in the SDM analysis 
        taxa_df   <- st_as_sf(analysis_df) %>% 
          filter(., searchTaxon == taxa | !!sym(taxa_level) == taxa)
        
        ## Buffer thet points by Xkm
        message('buffer SDM points for ', taxa)
        taxa_buffer <- st_buffer(taxa_df, dist = buffer) %>% 
          st_set_crs(., epsg)
        
        ## If the taxa don't intersect with the veg layer, we need an exception there
        
        ## Clip the habitat polygon by the 50km buffer
        message('Clip habitat layer to the SDM points for ', taxa)
        taxa_VEG_intersects_clip <- st_intersection(taxa_buffer, habitat_poly) %>% 
          dplyr::select(all_of(int_cols))
        
        gc()
        
        if(nrow(taxa_VEG_intersects_clip) > 0 ) {
          
          ## Intersect clipped habitat with buffer
          ## do we need another exception here?
          message('Intersect taxa df with Vegetation for ', taxa)
          
          ## Save intersection as a raster
          ## Set the ncol/nrow to match 100m resolutions
          if(raster_convert) {
            
            message('convert shapefile to raster for ', taxa)
            extent   <- extent(taxa_VEG_intersects_clip)
            x_length <- (extent[2] - extent[1])/100
            x_length <- round(x_length)
            y_length <- (extent[4] - extent[3])/100
            y_length <- round(y_length)
            
            ## Set the values to 1 : any veg within xkm is considered decent habitat
            r          <- raster(ncol = x_length, nrow = y_length)
            extent(r)  <- extent
            taxa_VEG_intersects_raster <- terra::rasterize(taxa_VEG_intersects_clip, r)
            taxa_VEG_intersects_raster[taxa_VEG_intersects_raster > 0] <- 1
            taxa_VEG_intersects_raster[taxa_VEG_intersects_raster < 0] <- 1
            
            gc()
            
            writeRaster(taxa_VEG_intersects_raster, 
                        paste0(output_path, save_name, '_VEG_intersection_', buffer, 'm.tif'),
                        overwrite = TRUE)
            
          }
          
          ## Raster intersect :: doesn't work because the LUT is not working
          ## Get the cells from the raster at those points
          if(save_shp) {
            
            st_write(taxa_VEG_intersects_clip %>% st_as_sf(), 
                     paste0(output_path, save_name, '_VEG_intersection.shp'))
          }
          
          message('Writing SDM + Veg intersect for ', taxa)
          st_write(taxa_VEG_intersects_clip %>% st_as_sf(), 
                   
                   dsn    = paste0(output_path, save_name, '_SDM_VEG_intersection.gpkg'), 
                   layer  = paste0(taxa, '_VEG_intersection'), 
                   quiet  = TRUE, 
                   append = FALSE)
          
          ## Save the taxa * habitat intersection as a raster
          message('writing threshold png for ', taxa)
          if(save_png){
            png(paste0(output_path, save_name, "_VEG_intersection.png"),
                16, 10, units = 'in', res = 500)
            
            ##
            plot(st_geometry(taxa_VEG_intersects_clip), 
                 main = paste0(taxa, ' Veg Intersection'), col = "red")
            
            # plot(taxa_df, add = TRUE, col = "red",   lwd = 1)
            plot(poly, add = TRUE)
            dev.off()}
          
          ## Save in two places, in the taxa folder, 
          ## and in the habitat suitability folder
          gc()
          
        } else {
          message('Habitat does not intersect with ', taxa, ' skip')
        }
        
      } else {
        message('Habitat-Veg intersect already done for ', taxa, ' skip')
      }
      
    } else {
      message('Skip habitat intersect for ', taxa, ' no data')
    }
    
  })
}





#' @title Intersect habitat suitability raster layers with other raster layer (e.g. Fire).
#' @description Takes a habitat suitability layer, and intersects it with a fire suitability layer.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param targ_maxent_table  data frame - table of maxent results for target taxa
#' @param host_maxent_table  data frame - table of maxent results for host taxa
#' @param target_path        Character string - The file path containing the existing maxent models
#' @param intersect_path     Character string - The file path containing the intersecting rasters
#' @param raster_pattern     Character string - The pattern to look for of Invertebrate rasters
#' @param targ_maxent_table  Data frame       - A table of maxent results to be used for mapping 
#' @param cell_size          Numeric          - Cell size to resample output
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @export calculate_taxa_habitat_host_rasters
calculate_taxa_habitat_host_rasters = function(taxa_list,
                                               targ_maxent_table,
                                               host_maxent_table,
                                               target_path,
                                               output_path,
                                               intersect_path,
                                               raster_pattern,
                                               fire_raster,
                                               cell_size,
                                               fire_thresh,
                                               write_rasters,
                                               poly_path,
                                               epsg) {
  
  ## Get the AUS shapefile
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[74]
    lapply(function(taxa) {
      
      
      if(host_maxent_table != 'NONE') {
        
        ## Get the directory of the host plants
        host_dir <- targ_maxent_table %>%
          filter(searchTaxon == taxa) %>%
          dplyr::select(host_dir)     %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the directory of the host plants
        host_taxa <- targ_maxent_table    %>%
          filter(searchTaxon == taxa)     %>%
          dplyr::select(HostTaxon) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the sdm threshold for each host taxa
        host_thresh <- host_maxent_table    %>%
          filter(searchTaxon == host_taxa)  %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the taxa directory name
        save_name <- gsub(' ', '_', taxa)
        host_name <- gsub(' ', '_', host_taxa)
        
      }
      
      ## Get the sdm threshold for each inv taxa
      target_thresh <- targ_maxent_table  %>%
        filter(searchTaxon == taxa)       %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      host_dir <- NA
      
      ## Get the taxa directory name
      save_name <- gsub(' ', '_', taxa)
      
      current_thresh = sprintf('%s/%s/full/%s_%s%s.tif', target_path,
                               save_name, save_name, 
                               "current_suit_not_novel_above_", target_thresh)
      
      ## If the invert taxa has a host plant, use the SDM from the host plant
      if(is.na(host_dir)) {
        
        ## If the threshold raster data doesn't exist :
        if(file.exists(current_thresh)) {
          
          ## Print the taxa being analysed
          message('Intersecting SDM with Fire for', taxa, ' | Logistic > ', target_thresh)
          
          ## Read in the current suitability raster :: get the current_not_novel raster
          sdm_threshold    <- raster(current_thresh)
          
          ## Read the SVTM intersect file in
          intersect_file <- list.files(intersect_path, 
                                       pattern = int_patt, 
                                       full.names = TRUE) %>% 
            .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' but it does not have a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", 
                                              exent = extent(sdm_threshold))
            
            ##
            message('mosaic Veg and SDM rasters for ', taxa)
            sdm_plus_veg <- raster::mosaic(sdm_threshold, intersect_sdm, fun = max)
            
            ## Multiply the SDM raster by the Fire Raster
            message('multiply habitat raster by the fire raster')
            sdm_plus_veg_intersect_fire <- sdm_plus_veg * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxon's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_plus_veg, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            gc()
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))  %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            
            gc()
            
            ## Save the % burnt layers..
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxon/threshold
            writeRaster(sdm_plus_veg, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  fire_raster,
                                  sdm_plus_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'Spectral'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDM + Veg', 'Fire', ' [SDM + Veg] * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
            dev.off()
            
            gc()
            
          } else {
            message('SDM and Veg rasters do not intersect for ', taxa, ' and it does not have a host taxa')
            
            ## Multiply the SDM raster by the Fire Raster
            message('multiply habitat raster by the fire raster')
            sdm_intersect_fire <- sdm_threshold * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxon's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_threshold, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>% 
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            writeRaster(sdm_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_threshold,
                                  fire_raster,
                                  sdm_intersect_fire,
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDM', 'Fire', 'SDM * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
            dev.off()
            
            gc()
            
          }
          
        } else {
          message('Habitat Suitability threshold raster does not exist for', taxa, ' skip')
          cat(taxa)
        }
        
        
      } else {
        message(taxa, ' has a host plant, use both SDMs')
        
        ## Print the taxa being analysed
        message('calculating habitat * fire for ', taxa, ' | Logistic > ', target_thresh)
        host_threshold <- paste0(host_dir, host_name, "_current_suit_not_novel_above_", host_thresh, '.tif')
        
        if(file.exists(host_threshold)) {
          
          ## Read in host and target rasters
          host_threshold <- raster(host_threshold)
          sdm_threshold  <- raster(current_thresh)
          
          ## Read the SVTM intersect file in
          intersect_file <- list.files(intersect_path, pattern = raster_pattern, full.names = TRUE) %>% 
            .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' and it has a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", exent = extent(sdm_threshold))
            gc()
            
            ##
            message('mosaicing Veg, host and target SDM rasters for ', taxa)
            sdm_plus_host_veg <- raster::mosaic(sdm_threshold, host_threshold, intersect_sdm, fun = max)
            
            ## Multiply the SDM raster by the Fire Raster
            message('Multiply Combo habitat raster by the fire raster')
            sdm_plus_veg_intersect_fire <- sdm_plus_host_veg * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxon's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_plus_host_veg, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxon/threshold
            writeRaster(sdm_plus_host_veg, 
                        paste0(output_path, save_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  fire_raster,
                                  sdm_plus_veg_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuous
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs + Veg', 'Fire', ' [SDMs + Veg] * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
            dev.off()
            
            gc()
            
          } else {
            message('SDM and Veg rasters do not intersect for ', taxa, 'but it has a host taxa')
            
            message('mosaicing host and target SDM rasters for ', taxa)
            sdm_plus_host <- raster::mosaic(sdm_threshold, host_threshold, fun = max)
            
            ## Multiply the SDM raster by the Fire Raster
            message('multiply habitat raster by the fire raster')
            sdm_plus_host_intersect_fire <- sdm_plus_host * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxon's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_plus_host, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            gc()
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2          = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_Host_intersect_Fire.csv'), row.names = FALSE)
            
            ## Write the current suitability raster, thresholded using the Maximum training
            ## sensitivity plus specificity Logistic threshold
            message('Writing ', taxa, ' SDM * Fire rasdters', ' max train > ', target_thresh)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxon/threshold
            writeRaster(sdm_plus_host, 
                        paste0(output_path, save_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            ## Save in two places, in the taxa folder, 
            ## and in the habitat suitability folder
            writeRaster(sdm_plus_host_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_Host_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing SDM * FIRE png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_Host_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  sdm_plus_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs', ' [SDMs * Fire]'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference...
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly)))
            
            dev.off()
            
            gc()
            
          }
          
        } else {
          message('Habitat Suitability threshold raster does not exist for', taxa, ' skip')
          cat(taxa)
        }
        
      }
    }) 
}






#' @title Intersect habitat suitability feature layers with other feature layer (e.g. Fire).
#' @description Takes a habitat suitability layer, and intersects it with a fire suitability layer.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param targ_maxent_table  data frame - table of maxent results for target taxa
#' @param host_maxent_table  data frame - table of maxent results for host taxa
#' @param target_path        Character string - The file path containing the existing maxent models
#' @param thresh_path        Character string - The file path containing the existing maxent models
#' @param output_path        Character string - The file path to save the function output
#' @param host_path          Character string - The file path to save the function output
#' @param intersect_path     Character string - The file path containing the intersecting rasters
#' @param intersect_patt     Character string - The pattern for the intersect files
#' @param int_cols           Character string - List of intersect columns
#' @param thresh_patt        Character string - The pattern for the threshold files
#' @param main_int_layer     Character string - The pattern for the intersect files
#' @param second_int_layer   Character string - The pattern for the intersect files
#' @param template_raster    Raster::raster - template raster with study extent and resolution
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @export calculate_taxa_habitat_host_features
calculate_taxa_habitat_host_features = function(taxa_list,
                                                targ_maxent_table,
                                                host_maxent_table,
                                                target_path,
                                                output_path,
                                                thresh_path,
                                                host_path,
                                                intersect_path,
                                                intersect_patt,
                                                int_cols,
                                                thresh_patt,
                                                main_int_layer,
                                                second_int_layer,
                                                template_raster, 
                                                poly_path,
                                                epsg) {
  
  ## Get the AUS shapefile
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  message('Use GEOS geometry for sf operations to speed up intersections')
  sf_use_s2(FALSE)
  million_metres <- 1000000
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[52]
    # Mutusca brevicornis
    lapply(function(taxa) {
      
      ## Get table
      target_table <- targ_maxent_table %>%
        filter(searchTaxon == taxa)
      
      host_count <- target_table %>% dplyr::select(HostTaxon, 
                                                   HostTaxon2, 
                                                   HostTaxon3, 
                                                   HostTaxon4) %>%  
        summarise_all(funs(sum(!is.na(.)))) %>% rowSums()
      
      if(!is.na(target_table$HostTaxon)) {
        
        ## Get the directory of the host plants
        host_dir <- host_maxent_table %>%
          filter(searchTaxon == taxa) %>%
          dplyr::select(host_dir)  %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the directory of the host plants
        host_taxa <- host_maxent_table    %>%
          filter(searchTaxon == taxa)     %>%
          dplyr::select(HostTaxon) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the sdm threshold for each host taxa
        host_thresh <- host_maxent_table  %>%
          filter(HostTaxon == host_taxa)  %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the sdm threshold for each inv taxa
        target_thresh <- targ_maxent_table  %>%
          filter(searchTaxon == taxa)       %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the taxa directory name
        save_name <- gsub(' ', '_', taxa)
        host_name <- gsub(' ', '_', host_taxa)
        
      } else {
        
        ## Get the directory of the host plants
        host_dir    <- NA
        host_taxa   <- NA
        host_thresh <- NA
        
        ## Get the sdm threshold for each inv taxa
        target_thresh <- targ_maxent_table  %>%
          filter(searchTaxon == taxa)       %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the taxa directory name
        save_name <- gsub(' ', '_', taxa)
        host_name <- NA
        
      }
      
      if(!is.na(target_table$HostTaxon2)) {
        
        ## Get the directory of the host plants
        host_dir2 <- host_maxent_table   %>%
          filter(HostTaxon == target_table$HostTaxon2) %>%
          dplyr::select(host_dir)  %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the directory of the host plants
        host_taxa2 <- host_maxent_table  %>%
          filter(HostTaxon == host_taxa) %>%
          dplyr::select(HostTaxon2) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the sdm threshold for each host taxa
        host_thresh2 <- host_maxent_table %>%
          filter(HostTaxon == host_taxa2) %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        host_name2 <- gsub(' ', '_', host_taxa2)
        
      } else {
        ## Get the directory of the host plants
        host_dir2    <- NA
        host_taxa2   <- NA
        host_thresh2 <- NA
        host_name2   <- NA
      }
      
      if(!is.na(target_table$HostTaxon3)) {
        
        ## Get the directory of the host plants
        host_dir3 <- host_maxent_table   %>%
          filter(HostTaxon == target_table$HostTaxon3) %>%
          dplyr::select(host_dir)  %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the directory of the host plants
        host_taxa3 <- host_maxent_table  %>%
          filter(HostTaxon == host_taxa) %>%
          dplyr::select(HostTaxon3) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the sdm threshold for each host taxa
        host_thresh3 <- host_maxent_table %>%
          filter(HostTaxon == host_taxa3) %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        host_name3 <- gsub(' ', '_', host_taxa3)
        
      } else {
        ## Get the directory of the host plants
        host_dir3    <- NA
        host_taxa3   <- NA
        host_thresh3 <- NA
        host_name3   <- NA
      }
      
      if(!is.na(target_table$HostTaxon4)) {
        
        ## Get the directory of the host plants
        host_dir4 <- host_maxent_table   %>%
          filter(HostTaxon == target_table$HostTaxon4) %>%
          dplyr::select(host_dir)  %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the directory of the host plants
        host_taxa4 <- host_maxent_table  %>%
          filter(HostTaxon == host_taxa) %>%
          dplyr::select(HostTaxon4) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the sdm threshold for each host taxa
        host_thresh3 <- host_maxent_table %>%
          filter(HostTaxon == host_taxa3) %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        host_name4 <- gsub(' ', '_', host_taxa4)
        
      } else {
        ## Get the directory of the host plants
        host_dir4    <- NA
        host_taxa4   <- NA
        host_thresh4 <- NA
        host_name4   <- NA
      }
      
      current_thresh_feat_path <- list.files(path       = thresh_path,
                                             pattern    = '_current_suit_not_novel_above_', 
                                             recursive  = TRUE,
                                             full.names = TRUE) %>% .[grep(".gpkg", .)] %>% .[grep(save_name, .)]
      
      if(length(current_thresh_feat_path) != 0) {
        
        occ                <- readRDS(sprintf('%s/%s/%s_occ.rds', 
                                              target_path, save_name, save_name))
        
        sdm_fire_geo       <- paste0(output_path, save_name, '_sdm_fire_intersect.gpkg')
        sdm_fire_png       <- paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png')
        
        if(!file.exists(sdm_fire_png)) {
          
          ## Read in the current suitability raster :: get the current_not`_novel raster
          ## Get the taxa directory name.
          sdm_threshold <- st_read(dsn = current_thresh_feat_path)
          
          extent_dim    <- extent(sdm_threshold)[1]
          
          if(!is.na(extent_dim)) {
            
            sdm_threshold_cast <- sdm_threshold %>% 
              st_cast(., "POLYGON") %>% 
              
              ## This hasn't burnt yet
              mutate(Taxa     = taxa)
            
            sdm_threshold_att <- sdm_threshold %>% 
              st_cast(., "POLYGON") %>% 
              
              ## This hasn't burnt yet
              mutate(Habitat  = 1,
                     Taxa     = taxa,
                     Area_km2 = st_area(geom)/million_metres,
                     Area_km2 = drop_units(Area_km2)) %>% 
              dplyr::select(Habitat, Taxa, Area_km2) 
            
            ## If the invert taxa has no host ----
            if(is.na(host_dir)) {
              
              ## Read the SVTM intersect file in
              intersect_string <- list.files(intersect_path, 
                                             pattern    = intersect_patt, 
                                             full.names = TRUE) %>% 
                .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
              
              ## Veg intersect, No host ---- 
              if(length(intersect_string) == 1) {
                
                message('SDM and Veg rasters intersect for ', taxa, ' but it does not have a host taxa')
                ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
                ## If FIRE is NA, that means that....
                intersect_file <- st_read(dsn = intersect_string) %>% 
                  filter(!st_is_empty(.)) %>% 
                  repair_geometry() %>% 
                  dplyr::select(all_of(int_cols))
                
                ## Now combine the Veg intersect and the SDM
                message('merging SDMs and Veg data for ', taxa)
                single_sf         <- dplyr::bind_rows(list(sdm_threshold_att, intersect_file))
                sdm_plus_veg      <- st_union(single_sf)
                sdm_plus_veg_poly <- st_cast(sdm_plus_veg, "POLYGON")
                
                sdm_plus_veg_att  <- sdm_plus_veg_poly %>% st_as_sf() %>%
                  
                  dplyr::mutate(Habitat       = 1,
                                Taxa          = taxa,
                                Area_Poly_km2 = st_area(x)/million_metres,
                                Area_Poly_km2 = drop_units(Area_Poly_km2))
                
                ## Calculate area of the SDM - don't need the fire area
                sdm_areas     <- st_area(sdm_plus_veg_att)/million_metres
                sdm_areas_km2 <- drop_units(sdm_areas) 
                sdm_area_km2  <- drop_units(sdm_areas) %>% sum()
                gc()
                
                ## Then do the Cell stats ::
                ## estimated x % of each taxon's habitat in each fire intensity category 
                message('Intersecting SDM with Fire layers for ', taxa)
                sdm_fire_int           <- st_intersection(sdm_plus_veg_att, main_int_layer)   
                sdm_fire_areas         <- st_area(sdm_fire_int)/million_metres
                sdm_fire_areas_km2     <- drop_units(sdm_fire_areas)
                sdm_fire_area_km2      <- drop_units(sdm_fire_areas) %>% sum()
                gc()
                
                ## create sf attributes for each intersecting polygon
                sdm_fire_int_att <- sdm_fire_int %>% st_cast(., "POLYGON") %>%
                  
                  mutate(Habitat       = 1,
                         Taxa          = taxa,
                         Fire          = 'Burnt',
                         Area_Poly_km2 = st_area(x)/million_metres,
                         Area_Poly_km2 = drop_units(Area_Poly_km2))
                
                ## Calculate total areas separately
                sdm_fire_int_areas_m2  <- st_area(sdm_fire_int)/million_metres
                sdm_fire_int_areas_km2 <- drop_units(sdm_fire_int_areas_m2)
                sdm_fire_int_area_km2  <- drop_units(sdm_fire_int_areas_m2) %>% sum() 
                percent_burnt_overall  <- sdm_fire_int_area_km2/sdm_area_km2 * 100 %>% round(.)
                gc()
                
                ## calc % burnt within forest
                message('Intersecting SDM + Fire layer with Forest layer for ', taxa)
                sdm_fire_forest_int       <- st_intersection(sdm_fire_int_att,   second_int_layer)
                
                sdm_fire_forest_areas_m2  <- st_area(sdm_fire_forest_int)/million_metres
                sdm_fire_forest_areas_km2 <- drop_units(sdm_fire_forest_areas_m2)
                sdm_fire_forest_area_km2  <- drop_units(sdm_fire_forest_areas_m2) %>% sum()
                gc()
                
                ## create sf attributes for each intersecting polygon
                sdm_fire_forest_int_att <- sdm_fire_forest_int %>% 
                  
                  mutate(Taxa              = taxa,
                         Fire              = 'Burnt',
                         Area_poly         = st_area(x)/million_metres,
                         Area_poly         = drop_units(Area_poly),
                         Percent_burnt_veg = (Area_poly/sdm_area_km2 * 100 %>% round(., 1)))
                
                ## Aggregate the sdm * fire * forest areas into veg classes
                sdm_fire_forest_int_classes <- sdm_fire_forest_int_att %>%
                  
                  st_set_geometry(NULL) %>% 
                  dplyr::select(Taxa, Vegetation, Area_poly, Percent_burnt_veg) %>% 
                  group_by(Taxa, Vegetation) %>% 
                  summarise(Area_poly         = sum(Area_poly),
                            Percent_burnt_veg = sum(Percent_burnt_veg))
                
                ## calc % burnt within forest classes
                percent_burnt_forest_overall <- sum(sdm_fire_forest_int_classes$Percent_burnt_veg)
                percent_burnt_forest_class   <- sdm_fire_forest_int_classes$Area_poly/
                  sdm_fire_forest_area_km2 * 100 %>% round(., 1)
                
                ## Create a tibble of overall areas for each taxon
                ## Include the SDM area in each fire classs here
                sdm_fire_overall_areas <- data.frame(matrix(NA, ncol = 4, nrow = 1))
                
                colnames(sdm_fire_overall_areas) <- c('Taxa', 
                                                      'Habitat_km2', 
                                                      'Habitat_burnt_km2', 
                                                      'Percent_burnt')
                
                sdm_fire_overall_areas <- sdm_fire_overall_areas %>% 
                  
                  mutate(Taxa              = taxa,
                         Habitat_km2       = sdm_area_km2,
                         Habitat_burnt_km2 = sdm_fire_int_area_km2,
                         Percent_burnt     = percent_burnt_overall)
                
                ## Create a tibble of vegetation areas for each taxon
                veg_length <- unique(sdm_fire_forest_int$Vegetation) %>% length
                sdm_fire_forest_areas <- data.frame(matrix(NA, 
                                                           ncol = 4, 
                                                           nrow = veg_length))
                
                colnames(sdm_fire_forest_areas) <- c('Taxa', 
                                                     'Vegetation', 
                                                     'Habitat_Veg_burnt_area',  
                                                     'Habitat_Veg_burnt_perc')
                
                sdm_fire_forest_areas <- sdm_fire_forest_areas %>% 
                  
                  mutate(Taxa                   = taxa,
                         Vegetation             = unique(sdm_fire_forest_int$Vegetation),
                         Habitat_Veg_burnt_area = sdm_fire_forest_int_classes$Area_poly,
                         Habitat_Veg_burnt_perc = percent_burnt_forest_class)
                
                ## Save the % burnt layers
                write.csv(sdm_fire_overall_areas, 
                          paste0(output_path, save_name, '_SDM_intersect_Fire.csv'),     row.names = FALSE)
                write.csv(sdm_fire_forest_areas,  
                          paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
                gc()
                
                ## Now save the thresh-holded rasters as shapefiles
                message('Saving SDM Fire intersect polygons for ', taxa)
                
                st_write(sdm_plus_veg_att, 
                         
                         dsn    = sdm_fire_geo, 
                         layer  = paste0(save_name, '_sdm_plus_veg_att'),
                         
                         quiet  = TRUE,
                         append = FALSE)
                gc()
                
                
                st_write(sdm_fire_int_att, 
                         
                         dsn    = sdm_fire_geo, 
                         layer  = paste0(save_name, '_sdm_fire_int_sub'),
                         
                         quiet  = TRUE,
                         append = FALSE)
                gc()
                
                st_write(sdm_fire_forest_int_att, 
                         
                         dsn    = sdm_fire_geo, 
                         layer  = paste0(save_name, '_sdm_fire_forest_int_sub'),
                         
                         quiet  = TRUE,
                         append = FALSE)
                gc()
                
                ## Create rasters for plotting
                t <- raster::raster(template_raster) %>% 
                  raster::crop(., extent(main_int_layer))
                
                current_thresh_ras <- current_thresh_ras %>% 
                  raster::crop(., extent(main_int_layer))
                
                fire_layer_ras   <- fasterize(main_int_layer %>% st_cast(., "POLYGON"), t) %>% 
                  raster::crop(., extent(main_int_layer))
                
                sdm_plus_veg_ras <- fasterize(sdm_plus_veg_att %>% st_cast(., "POLYGON"), t) %>% 
                  raster::crop(., extent(main_int_layer))
                
                message('writing threshold png for ', taxa)
                png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                    6, 12, units = 'in', res = 400)
                
                plot(fire_layer_ras,               col = 'orange',legend = FALSE)
                plot(sdm_plus_veg_ras, add = TRUE, col = 'green', legend = FALSE)
                plot(poly, add = TRUE)
                
                title(main = taxa, 
                      sub  = paste0(round(percent_burnt_overall, 2), 
                                    " % Suitable habitat Burnt"))
                dev.off()
                gc()
                
              } else {
                ## No intersect, No host ----
                message('SDM and Veg rasters do not intersect for ', taxa, ' and it does not have a host taxa')
                
                ## Calculate area of the SDM - don't need the fire area
                sdm_area_km2  <- sdm_threshold_att$Area_km2 %>% sum()
                
                ## Then do the Cell stats ::
                ## estimated x % of each taxons habitat in each fire intensity category 
                message('Intersecting SDM with Fire layers for ', taxa)
                sdm_fire_int           <- st_intersection(sdm_threshold_att, main_int_layer)  
                sdm_fire_areas         <- st_area(sdm_fire_int)/million_metres
                sdm_fire_areas_km2     <- drop_units(sdm_fire_areas)
                sdm_fire_area_km2      <- drop_units(sdm_fire_areas) %>% sum()
                gc()
                
                ## create sf attributes for each intersecting polygon
                sdm_fire_int_att <- sdm_fire_int %>% st_cast(., "POLYGON") %>%
                  
                  mutate(Habitat       = 1,
                         Taxa          = taxa,
                         Fire          = 'Burnt',
                         Area_Poly_km2 = st_area(geom)/million_metres,
                         Area_Poly_km2 = drop_units(Area_Poly_km2))
                
                ## Calculate total areas separately
                sdm_fire_int_areas_m2  <- st_area(sdm_fire_int)/million_metres
                sdm_fire_int_areas_km2 <- drop_units(sdm_fire_int_areas_m2)
                sdm_fire_int_area_km2  <- drop_units(sdm_fire_int_areas_m2) %>% sum() 
                percent_burnt_overall  <- sdm_fire_int_area_km2/sdm_area_km2 * 100 %>% round(.)
                gc()
                
                ## calc % burnt within forest
                message('Intersecting SDM + Fire layer with Forest layer for ', taxa)
                sdm_fire_forest_int       <- st_intersection(sdm_fire_int_att,   second_int_layer)
                
                sdm_fire_forest_areas_m2  <- st_area(sdm_fire_forest_int)/million_metres
                sdm_fire_forest_areas_km2 <- drop_units(sdm_fire_forest_areas_m2)
                sdm_fire_forest_area_km2  <- drop_units(sdm_fire_forest_areas_m2) %>% sum()
                gc()
                
                ## create sf attributes for each intersecting polygon
                sdm_fire_forest_int_att <- sdm_fire_forest_int %>% 
                  
                  mutate(Taxa              = taxa,
                         Fire              = 'Burnt',
                         Area_poly         = st_area(geom)/million_metres,
                         Area_poly         = drop_units(Area_poly),
                         Percent_burnt_veg = (Area_poly/sdm_area_km2 * 100 %>% round(., 1)))
                
                ## Aggregate the sdm * fire * forest areas into veg classes
                sdm_fire_forest_int_classes <- sdm_fire_forest_int_att %>%
                  
                  st_set_geometry(NULL) %>% 
                  dplyr::select(Taxa, Vegetation, Area_poly, Percent_burnt_veg) %>% 
                  group_by(Taxa, Vegetation) %>% 
                  summarise(Area_poly         = sum(Area_poly),
                            Percent_burnt_veg = sum(Percent_burnt_veg))
                
                ## calc % burnt within forest classes
                percent_burnt_forest_overall <- sum(sdm_fire_forest_int_classes$Percent_burnt_veg)
                percent_burnt_forest_class   <- sdm_fire_forest_int_classes$Area_poly/
                  sdm_fire_forest_area_km2 * 100 %>% round(., 1)
                
                ## Create a tibble of overall areas for each taxon
                ## Include the SDM area in each fire classs here
                sdm_fire_overall_areas <- data.frame(matrix(NA, ncol = 4, nrow = 1))
                
                colnames(sdm_fire_overall_areas) <- c('Taxa', 
                                                      'Habitat_km2', 
                                                      'Habitat_burnt_km2', 
                                                      'Percent_burnt')
                
                sdm_fire_overall_areas <- sdm_fire_overall_areas %>% 
                  
                  mutate(Taxa              = taxa,
                         Habitat_km2       = sdm_area_km2,
                         Habitat_burnt_km2 = sdm_fire_int_area_km2,
                         Percent_burnt     = percent_burnt_overall)
                
                ## Create a tibble of vegetation areas for each taxon
                veg_length <- unique(sdm_fire_forest_int$Vegetation) %>% length
                sdm_fire_forest_areas <- data.frame(matrix(NA, 
                                                           ncol = 4, 
                                                           nrow = veg_length))
                
                colnames(sdm_fire_forest_areas) <- c('Taxa', 
                                                     'Vegetation', 
                                                     'Habitat_Veg_burnt_area',  
                                                     'Habitat_Veg_burnt_perc')
                
                sdm_fire_forest_areas <- sdm_fire_forest_areas %>% 
                  
                  mutate(Taxa                   = taxa,
                         Vegetation             = unique(sdm_fire_forest_int$Vegetation),
                         Habitat_Veg_burnt_area = sdm_fire_forest_int_classes$Area_poly,
                         Habitat_Veg_burnt_perc = percent_burnt_forest_class)
                
                ## Save the % burnt layers
                write.csv(sdm_fire_overall_areas, 
                          paste0(output_path, save_name, '_SDM_intersect_Fire.csv'),     row.names = FALSE)
                write.csv(sdm_fire_forest_areas,  
                          paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
                gc()
                
                ## Now save the thresh-holded rasters as shapefiles
                message('Saving SDM Fire intersect polygons for ', taxa)
                st_write(sdm_fire_int_att, 
                         
                         dsn    = sdm_fire_geo, 
                         layer  = paste0(save_name, '_sdm_fire_int_sub'),
                         
                         quiet  = TRUE,
                         append = FALSE)
                gc()
                
                st_write(sdm_fire_forest_int_att, 
                         
                         dsn    = sdm_fire_geo, 
                         layer  = paste0(save_name, '_sdm_fire_forest_int_sub'),
                         
                         quiet  = TRUE,
                         append = FALSE)
                gc()
                
                ## Create rasters for plotting
                t <- raster::raster(template_raster) %>% 
                  raster::crop(., extent(main_int_layer))
                
                current_thresh_ras <- current_thresh_ras %>% 
                  raster::crop(., extent(main_int_layer))
                
                fire_layer_ras <- fasterize(main_int_layer %>% st_cast(., "POLYGON"), t) %>% 
                  raster::crop(., extent(main_int_layer))
                
                sdm_plus_veg_ras <- fasterize(sdm_plus_veg_att %>% st_cast(., "POLYGON"), t) %>% 
                  raster::crop(., extent(main_int_layer))
                
                message('writing threshold png for ', taxa)
                png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                    6, 12, units = 'in', res = 400)
                
                plot(fire_layer_ras,               col = 'orange',legend = FALSE)
                plot(sdm_plus_veg_ras, add = TRUE, col = 'green', legend = FALSE)
                plot(poly, add = TRUE)
                
                title(main = taxa, 
                      sub  = paste0(round(percent_burnt_overall, 2), 
                                    " % Suitable habitat Burnt"))
                dev.off()
                gc()
                
              }
              
            } else {
              
              ## If the invert taxa has a host ----
              message(taxa, ' has ', host_count, ' host plants')
              
              ## Print the taxa being analysed
              host_threshold_ras <- paste0(host_dir, host_name, 
                                           "_current_suit_not_novel_above_", host_thresh, '.tif')
              
              host_string        <- list.files(host_path, 
                                               full.names = TRUE) %>% 
                .[grep(".gpkg", .)] %>% 
                
                .[grep(paste0(host_name, collapse = '|'), ., ignore.case = TRUE)]
              
              
              intersect_string   <- list.files(intersect_path, 
                                               pattern    = intersect_patt, 
                                               full.names = TRUE) %>% 
                .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
              
              host_threshold  <- st_read(dsn = host_string) %>% 
                filter(!st_is_empty(.)) %>% 
                repair_geometry()
              
              ## This block above needs to replicated for all host taxa for the target
              if(!is.na(host_dir2)) {
                
                host_threshold_ras2 <- paste0(host_dir2, host_name2, 
                                              "_current_suit_not_novel_above_", host_thresh2, '.tif')
                
                host_string2        <- list.files(host_dir2, 
                                                  "_current_suit_not_novel_above_",
                                                  full.names = TRUE) %>% 
                  .[grep(".gpkg", .)] %>% 
                  
                  .[grep(paste0(host_name2, collapse = '|'), ., ignore.case = TRUE)]
                
                host_threshold2  <- st_read(dsn = host_string2) %>% 
                  filter(!st_is_empty(.)) %>% 
                  repair_geometry()
                
              }
              
              if(!is.na(host_dir3)) {
                
                host_threshold_ras3 <- paste0(host_dir3, host_name3, 
                                              "_current_suit_not_novel_above_", host_thresh3, '.tif')
                
                host_string3        <- list.files(host_dir3, 
                                                  "_current_suit_not_novel_above_",
                                                  full.names = TRUE) %>% 
                  .[grep(".gpkg", .)] %>% 
                  
                  .[grep(paste0(host_name3, collapse = '|'), ., ignore.case = TRUE)]
                
                host_threshold3  <- st_read(dsn = host_string3) %>% 
                  filter(!st_is_empty(.)) %>% 
                  repair_geometry()
                
              }
              
              if(!is.na(host_dir4)) {
                
                host_threshold_ras4 <- paste0(host_dir4, host_name4, 
                                              "_current_suit_not_novel_above_", host_thresh4, '.tif')
                
                host_string4        <- list.files(host_dir4, 
                                                  "_current_suit_not_novel_above_",
                                                  full.names = TRUE) %>% 
                  .[grep(".gpkg", .)] %>% 
                  
                  .[grep(paste0(host_name4, collapse = '|'), ., ignore.case = TRUE)]
                
                host_threshold4  <- st_read(dsn = host_string4) %>% 
                  filter(!st_is_empty(.)) %>% 
                  repair_geometry()
                
              }
              
              if(file.exists(host_threshold_ras)) {
                
                if(length(intersect_string) == 1) {
                  
                  ## Veg intersect & host plant ---- 
                  message('SDM and Veg rasters intersect for ', taxa, ' and it has a host plant')
                  ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
                  ## If FIRE is NA, that means that....
                  intersect_file <- st_read(dsn = intersect_string) %>% 
                    filter(!st_is_empty(.)) %>% 
                    repair_geometry() %>% 
                    dplyr::select(all_of(int_cols))
                  
                  ## Now combine the Veg intersect and the SDM
                  message('merging SDMs and Veg data for ', taxa)
                  ## There needs to be variable for how many host there are, then a separate 
                  ## block for one, two, three or four hosts, each with their own results.
                  if(host_count == 1) {
                    
                    single_sf <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                       host_threshold, 
                                                       intersect_file))
                  }
                  
                  if(host_count == 2) {
                    
                    single_sf  <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                        host_threshold,
                                                        host_threshold2,
                                                        intersect_file))
                  }
                  
                  if(host_count == 3) {
                    
                    single_sf  <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                        host_threshold,
                                                        host_threshold2,
                                                        host_threshold3,
                                                        intersect_file))
                  }
                  
                  if(host_count == 4) {
                    
                    single_sf  <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                        host_threshold,
                                                        host_threshold2,
                                                        host_threshold3,
                                                        host_threshold4,
                                                        intersect_file))
                  }
                  
                  
                  sdm_plus_veg_host      <- st_union(single_sf)
                  sdm_plus_veg_host_poly <- st_cast(sdm_plus_veg_host, "POLYGON")
                  
                  sdm_plus_veg_att       <- sdm_plus_veg_host_poly %>% st_as_sf() %>%
                    
                    dplyr::mutate(Habitat       = 1,
                                  Taxa          = taxa,
                                  Area_Poly_km2 = st_area(x)/million_metres,
                                  Area_Poly_km2 = drop_units(Area_Poly_km2)) %>%
                    st_cast(., "POLYGON")
                  
                  ## Calculate area of the SDM - don't need the fire area
                  sdm_areas     <- st_area(sdm_plus_veg_att)/million_metres
                  sdm_areas_km2 <- drop_units(sdm_areas) 
                  sdm_area_km2  <- drop_units(sdm_areas) %>% sum()
                  
                  ## Then do the Cell stats ::
                  ## estimated x % of each taxon's habitat in each fire intensity category 
                  message('Intersecting SDM with Fire layers for ', taxa)
                  sdm_fire_int           <- st_intersection(sdm_plus_veg_att, main_int_layer)  
                  sdm_fire_areas         <- st_area(sdm_fire_int)/million_metres
                  sdm_fire_areas_km2     <- drop_units(sdm_fire_areas)
                  sdm_fire_area_km2      <- drop_units(sdm_fire_areas) %>% sum()
                  gc()
                  
                  ## create sf attributes for each intersecting polygon
                  sdm_fire_int_att <- sdm_fire_int %>% st_cast(., "POLYGON") %>%
                    
                    mutate(Habitat       = 1,
                           Taxa          = taxa,
                           Fire          = 'Burnt',
                           Area_Poly_km2 = st_area(x)/million_metres,
                           Area_Poly_km2 = drop_units(Area_Poly_km2))
                  
                  ## Calculate total areas separately
                  sdm_fire_int_areas_m2  <- st_area(sdm_fire_int)/million_metres
                  sdm_fire_int_areas_km2 <- drop_units(sdm_fire_int_areas_m2)
                  sdm_fire_int_area_km2  <- drop_units(sdm_fire_int_areas_m2) %>% sum() 
                  percent_burnt_overall  <- sdm_fire_int_area_km2/sdm_area_km2 * 100 %>% round(.)
                  gc()
                  
                  ## calc % burnt within forest
                  message('Intersecting SDM + Fire layer with Forest layer for ', taxa)
                  sdm_fire_forest_int       <- st_intersection(sdm_fire_int_att,   second_int_layer)
                  
                  sdm_fire_forest_areas_m2  <- st_area(sdm_fire_forest_int)/million_metres
                  sdm_fire_forest_areas_km2 <- drop_units(sdm_fire_forest_areas_m2)
                  sdm_fire_forest_area_km2  <- drop_units(sdm_fire_forest_areas_m2) %>% sum()
                  gc()
                  
                  ## create sf attributes for each intersecting polygon
                  sdm_fire_forest_int_att <- sdm_fire_forest_int %>% 
                    
                    mutate(Taxa              = taxa,
                           Fire              = 'Burnt',
                           Area_poly         = st_area(x)/million_metres,
                           Area_poly         = drop_units(Area_poly),
                           Percent_burnt_veg = (Area_poly/sdm_area_km2 * 100 %>% round(., 1)))
                  
                  ## Aggregate the sdm * fire * forest areas into veg classes
                  sdm_fire_forest_int_classes <- sdm_fire_forest_int_att %>%
                    
                    st_set_geometry(NULL) %>% 
                    dplyr::select(Taxa, Vegetation, Area_poly, Percent_burnt_veg) %>% 
                    group_by(Taxa, Vegetation) %>% 
                    summarise(Area_poly         = sum(Area_poly),
                              Percent_burnt_veg = sum(Percent_burnt_veg))
                  
                  ## calc % burnt within forest classes
                  percent_burnt_forest_overall <- sum(sdm_fire_forest_int_classes$Percent_burnt_veg)
                  percent_burnt_forest_class   <- sdm_fire_forest_int_classes$Area_poly/sdm_fire_forest_area_km2 * 100 %>% round(., 1)
                  
                  ## Create a tibble of overall areas for each taxon
                  ## Include the SDM area in each fire classs here
                  sdm_fire_overall_areas <- data.frame(matrix(NA, ncol = 4, nrow = 1))
                  
                  colnames(sdm_fire_overall_areas) <- c('Taxa', 
                                                        'Habitat_km2', 
                                                        'Habitat_burnt_km2', 
                                                        'Percent_burnt')
                  
                  sdm_fire_overall_areas <- sdm_fire_overall_areas %>% 
                    
                    mutate(Taxa              = taxa,
                           Habitat_km2       = sdm_area_km2,
                           Habitat_burnt_km2 = sdm_fire_int_area_km2,
                           Percent_burnt     = percent_burnt_overall)
                  
                  ## Create a tibble of vegetation areas for each taxon
                  veg_length <- unique(sdm_fire_forest_int$Vegetation) %>% length
                  sdm_fire_forest_areas <- data.frame(matrix(NA, 
                                                             ncol = 4, 
                                                             nrow = veg_length))
                  
                  colnames(sdm_fire_forest_areas) <- c('Taxa', 
                                                       'Vegetation', 
                                                       'Habitat_Veg_burnt_area',  
                                                       'Habitat_Veg_burnt_perc')
                  
                  sdm_fire_forest_areas <- sdm_fire_forest_areas %>% 
                    
                    mutate(Taxa                   = taxa,
                           Vegetation             = unique(sdm_fire_forest_int$Vegetation),
                           Habitat_Veg_burnt_area = sdm_fire_forest_int_classes$Area_poly,
                           Habitat_Veg_burnt_perc = percent_burnt_forest_class)
                  
                  ## Save the % burnt layers
                  write.csv(sdm_fire_overall_areas, 
                            paste0(output_path, save_name, '_SDM_intersect_Fire.csv'),     row.names = FALSE)
                  write.csv(sdm_fire_forest_areas,  
                            paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
                  gc()
                  
                  ## Now save the thresh-holded rasters as shapefiles
                  message('Saving SDM Fire intersect polygons for ', taxa)
                  
                  st_write(sdm_plus_veg_att, 
                           
                           dsn    = sdm_fire_geo, 
                           layer  = paste0(save_name, '_sdm_plus_veg_att'),
                           
                           quiet  = TRUE,
                           append = FALSE)
                  gc()
                  
                  st_write(sdm_fire_int_att, 
                           
                           dsn    = sdm_fire_geo, 
                           layer  = paste0(save_name, '_sdm_fire_int_sub'),
                           
                           quiet  = TRUE,
                           append = FALSE)
                  gc()
                  
                  st_write(sdm_fire_forest_int_att, 
                           
                           dsn    = sdm_fire_geo, 
                           layer  = paste0(save_name, '_sdm_fire_forest_int_sub'),
                           
                           quiet  = TRUE,
                           append = FALSE)
                  gc()
                  
                  ## Create rasters for plotting
                  t <- raster::raster(template_raster) %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  current_thresh_ras <- current_thresh_ras %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  fire_layer_ras   <- fasterize(main_int_layer %>% st_cast(., "POLYGON"), t) %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  sdm_plus_veg_ras <- fasterize(sdm_plus_veg_att %>% st_cast(., "POLYGON"), t) %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  message('writing threshold png for ', taxa)
                  png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                      6, 12, units = 'in', res = 400)
                  
                  plot(fire_layer_ras,               col = 'orange',legend = FALSE)
                  plot(sdm_plus_veg_ras, add = TRUE, col = 'green', legend = FALSE)
                  plot(poly, add = TRUE)
                  
                  title(main = taxa, 
                        sub  = paste0(round(percent_burnt_overall, 2), 
                                      " % Suitable habitat Burnt"))
                  dev.off()
                  gc()
                  
                } else {
                  ## No intersect, but host plant ---- 
                  message('SDM and Veg rasters do not intersect for ', taxa, 'but it has a host taxa')
                  
                  ## Veg intersect & host plant ---- 
                  message('SDM and Veg rasters intersect for ', taxa, ' and it has a host plant')
                  ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
                  ## If FIRE is NA, that means that....
                  
                  ## Now combine the Veg intersect and the SDM
                  message('merging SDMs and Veg data for ', taxa)
                  ## There needs to be variable for how many host there are, then a separate 
                  ## block for one, two, three or four hosts, each with their own results.
                  if(host_count == 1) {
                    
                    single_sf <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                       host_threshold))
                  }
                  
                  if(host_count == 2) {
                    
                    single_sf  <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                        host_threshold,
                                                        host_threshold2))
                  }
                  
                  if(host_count == 3) {
                    
                    single_sf  <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                        host_threshold,
                                                        host_threshold2,
                                                        host_threshold3))
                  }
                  
                  if(host_count == 4) {
                    
                    single_sf  <- dplyr::bind_rows(list(sdm_threshold_att, 
                                                        host_threshold,
                                                        host_threshold2,
                                                        host_threshold3,
                                                        host_threshold4))
                  }
                  
                  sdm_plus_host      <- st_union(single_sf)
                  sdm_plus_host_poly <- st_cast(sdm_plus_host, "POLYGON")
                  
                  sdm_plus_veg_att   <- sdm_plus_host_poly %>% st_as_sf() %>%
                    
                    dplyr::mutate(Habitat       = 1,
                                  Taxa          = taxa,
                                  Area_Poly_km2 = st_area(x)/million_metres,
                                  Area_Poly_km2 = drop_units(Area_Poly_km2))
                  
                  ## Calculate area of the SDM - don't need the fire area
                  sdm_areas     <- st_area(sdm_plus_veg_att)/million_metres
                  sdm_areas_km2 <- drop_units(sdm_areas) 
                  sdm_area_km2  <- drop_units(sdm_areas) %>% sum()
                  
                  ## Then do the Cell stats ::
                  ## estimated x % of each taxon's habitat in each fire intensity category 
                  message('Intersecting SDM with Fire layers for ', taxa)
                  sdm_fire_int           <- st_intersection(sdm_plus_veg_att, main_int_layer)  
                  sdm_fire_areas         <- st_area(sdm_fire_int)/million_metres
                  sdm_fire_areas_km2     <- drop_units(sdm_fire_areas)
                  sdm_fire_area_km2      <- drop_units(sdm_fire_areas) %>% sum()
                  gc()
                  
                  ## create sf attributes for each intersecting polygon
                  sdm_fire_int_att <- sdm_fire_int %>% st_cast(., "POLYGON") %>%
                    
                    mutate(Habitat       = 1,
                           Taxa          = taxa,
                           Fire          = 'Burnt',
                           Area_Poly_km2 = st_area(x)/million_metres,
                           Area_Poly_km2 = drop_units(Area_Poly_km2))
                  
                  ## Calculate total areas separately
                  sdm_fire_int_areas_m2  <- st_area(sdm_fire_int)/million_metres
                  sdm_fire_int_areas_km2 <- drop_units(sdm_fire_int_areas_m2)
                  sdm_fire_int_area_km2  <- drop_units(sdm_fire_int_areas_m2) %>% sum() 
                  percent_burnt_overall  <- sdm_fire_int_area_km2/sdm_area_km2 * 100 %>% round(.)
                  gc()
                  
                  ## calc % burnt within forest
                  message('Intersecting SDM + Fire layer with Forest layer for ', taxa)
                  sdm_fire_forest_int       <- st_intersection(sdm_fire_int_att,   second_int_layer)
                  
                  sdm_fire_forest_areas_m2  <- st_area(sdm_fire_forest_int)/million_metres
                  sdm_fire_forest_areas_km2 <- drop_units(sdm_fire_forest_areas_m2)
                  sdm_fire_forest_area_km2  <- drop_units(sdm_fire_forest_areas_m2) %>% sum()
                  gc()
                  
                  ## create sf attributes for each intersecting polygon
                  sdm_fire_forest_int_att <- sdm_fire_forest_int %>% 
                    
                    mutate(Taxa              = taxa,
                           Fire              = 'Burnt',
                           Area_poly         = st_area(x)/million_metres,
                           Area_poly         = drop_units(Area_poly),
                           Percent_burnt_veg = (Area_poly/sdm_area_km2 * 100 %>% round(., 1)))
                  
                  ## Aggregate the sdm * fire * forest areas into veg classes
                  sdm_fire_forest_int_classes <- sdm_fire_forest_int_att %>%
                    
                    st_set_geometry(NULL) %>% 
                    dplyr::select(Taxa, Vegetation, Area_poly, Percent_burnt_veg) %>% 
                    group_by(Taxa, Vegetation) %>% 
                    summarise(Area_poly         = sum(Area_poly),
                              Percent_burnt_veg = sum(Percent_burnt_veg))
                  
                  ## calc % burnt within forest classes
                  percent_burnt_forest_overall <- sum(sdm_fire_forest_int_classes$Percent_burnt_veg)
                  percent_burnt_forest_class   <- sdm_fire_forest_int_classes$Area_poly/
                    sdm_fire_forest_area_km2 * 100 %>% round(., 1)
                  
                  ## Create a tibble of overall areas for each taxon
                  ## Include the SDM area in each fire classs here
                  sdm_fire_overall_areas <- data.frame(matrix(NA, ncol = 4, nrow = 1))
                  
                  colnames(sdm_fire_overall_areas) <- c('Taxa', 
                                                        'Habitat_km2', 
                                                        'Habitat_burnt_km2', 
                                                        'Percent_burnt')
                  
                  sdm_fire_overall_areas <- sdm_fire_overall_areas %>% 
                    
                    mutate(Taxa              = taxa,
                           Habitat_km2       = sdm_area_km2,
                           Habitat_burnt_km2 = sdm_fire_int_area_km2,
                           Percent_burnt     = percent_burnt_overall)
                  
                  ## Create a tibble of vegetation areas for each taxon
                  veg_length <- unique(sdm_fire_forest_int$Vegetation) %>% length
                  sdm_fire_forest_areas <- data.frame(matrix(NA, 
                                                             ncol = 4, 
                                                             nrow = veg_length))
                  
                  colnames(sdm_fire_forest_areas) <- c('Taxa', 
                                                       'Vegetation', 
                                                       'Habitat_Veg_burnt_area',  
                                                       'Habitat_Veg_burnt_perc')
                  
                  sdm_fire_forest_areas <- sdm_fire_forest_areas %>% 
                    
                    mutate(Taxa                   = taxa,
                           Vegetation             = unique(sdm_fire_forest_int$Vegetation),
                           Habitat_Veg_burnt_area = sdm_fire_forest_int_classes$Area_poly,
                           Habitat_Veg_burnt_perc = percent_burnt_forest_class)
                  
                  ## Save the % burnt layers
                  write.csv(sdm_fire_overall_areas, 
                            paste0(output_path, save_name, '_SDM_intersect_Fire.csv'),     row.names = FALSE)
                  write.csv(sdm_fire_forest_areas,  
                            paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
                  gc()
                  
                  ## Now save the thresh-holded rasters as shapefiles
                  message('Saving SDM Fire intersect polygons for ', taxa)
                  st_write(sdm_plus_veg_att, 
                           
                           dsn    = sdm_fire_geo, 
                           layer  = paste0(save_name, '_sdm_plus_veg_att'),
                           
                           quiet  = TRUE,
                           append = FALSE)
                  gc()
                  
                  st_write(sdm_fire_int_att, 
                           
                           dsn    = sdm_fire_geo, 
                           layer  = paste0(save_name, '_sdm_fire_int_sub'),
                           
                           quiet  = TRUE,
                           append = FALSE)
                  gc()
                  
                  st_write(sdm_fire_forest_int_att, 
                           
                           dsn    = sdm_fire_geo, 
                           layer  = paste0(save_name, '_sdm_fire_forest_int_sub'),
                           
                           quiet  = TRUE,
                           append = FALSE)
                  gc()
                  
                  ## Create rasters for plotting
                  t <- raster::raster(template_raster) %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  current_thresh_ras <- current_thresh_ras %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  fire_layer_ras   <- fasterize(main_int_layer %>% st_cast(., "POLYGON"), t) %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  sdm_plus_veg_ras <- fasterize(sdm_plus_veg_att %>% st_cast(., "POLYGON"), t) %>% 
                    raster::crop(., extent(main_int_layer))
                  
                  message('writing threshold png for ', taxa)
                  png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                      6, 12, units = 'in', res = 400)
                  
                  plot(fire_layer_ras,               col = 'orange',legend = FALSE)
                  plot(sdm_plus_veg_ras, add = TRUE, col = 'green', legend = FALSE)
                  plot(poly, add = TRUE)
                  
                  title(main = taxa, 
                        sub  = paste0(round(percent_burnt_overall, 2), 
                                      " % Suitable habitat Burnt"))
                  dev.off()
                  gc()
                }
                
              } else {
                message('Habitat Suitability threshold raster does not exist for', host_taxa, ' skip')
                cat(taxa)
              }
              
            }
            
          } else {
            message('SDM thresh is invalid for ', taxa, ' skip')
            cat(taxa)
          }
          
        } else {
          message('SDM-Fire intersect already performed for ', taxa, ' skip')
          cat(taxa)
        }
        
      } else {
        message('SDM thresh doesnt exist for ', taxa, ' skip')
        cat(taxa)
      }
      
    }) 
}





#' @title Create vector from a raster. 


#' @description This function takes a raster of a shapefile (for example the urban areas of Australia),
#' and creates a shapefile (i.e. a vector).
#' It uses the rmaxent package https://github.com/johnbaums/rmaxent
#' It assumes that the input df is that returned by the prepare_sdm_table function
#' 
#' @param shp_file           SpatialPolygonsDataFrame - Spdf of spatial units used to aggregate the SDMs (e.g. urban areas of Australia)
#' @param prj                CRS object - Local projection for mapping the shapefile (e.g. Australian Albers)
#' @param sort_var           Character string - The field name in the shapefile to use for sorting (e.g. Urban area names)
#' @param agg_var            Character string - The field name in the shapefile to use for aggregating SDM results (e.g. Urban area codes)
#' @param temp_ras           Raster - An existing raster with the same extent, resolution and projection as the maxent models (e.g. Australia)
#' @param targ_ras           Raster - An existing raster of the shapefile with the same extent, resolution and projection as the maxent models (e.g. Australia)
#' @export shapefile_vector_from_raster
shapefile_vector_from_raster = function (shp_file,
                                         prj,
                                         agg_var,
                                         temp_ras,
                                         targ_ras) {
  
  ## Read in shapefile
  areal_unit <- shp_file %>%
    spTransform(prj)
  
  ## Rasterize shapefile, insert 'sort_var'
  message('Rasterizing shapefile')
  #areal_unit = areal_unit[order(areal_unit$SUA_NAME16),]
  f <- tempfile()
  
  ## Write a temporary raster
  writeOGR(areal_unit, tempdir(), basename(f), 'ESRI Shapefile')
  template <- raster(temp_ras)
  
  ## Rasterize the shapefile
  areal_unit_rast <- gdalUtils::gdal_rasterize(
    normalizePath(paste0(f, '.shp')),
    
    ## The missing step is the .tiff, it was created somewhere else, in R or a GIS
    ## so 'a' needs to match in this step, and the next
    targ_ras, tr = res(template),
    te = c(bbox(template)), a = agg_var, a_nodata = 0, init = 0, ot = 'UInt16', output_Raster = TRUE)
  
  gc()
  
  areal_unit_vec <- c(areal_unit_rast[])
  summary(areal_unit_vec)
  
  ## return the vector
  return(areal_unit_vec)
  
}





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################
