############################################################################################
####################################  ------ DC1.5 SPECIES ---- ############################
############################################################################################


# \ 
# 
# This code analyzes the impact of extraction on the HEVAE
#
#   \





## ENVIRONMENT SETTINGS =============================================================


## To-do

## Re-format HEVAE Risk and Threatened species tables



if(threat_species) {
  
  ## If we are analyzing the species
  NSW_water_sources_HEVAE_Distinct_agg <- 
    
    NSW_water_sources_HEVAE_Distinct %>% 
    as.data.frame() %>% replace(is.na(.), 0)    %>% 
    group_by(CHR_Water_Source) %>% 
    summarise_if(is.numeric, max, na.rm = TRUE) %>% 
    
    ## Only get the species columns with data
    .[, colSums(. != 0) > 0] 
  
  
  ## Column names
  frog_cols <- dplyr::select(NSW_water_sources_HEVAE_Distinct_agg, 
                             contains("frog")) %>% names() %>% .[-1]
  
  fish_cols <- dplyr::select(NSW_water_sources_HEVAE_Distinct_agg, 
                             contains("fish")) %>% names() %>% .[-1]
  
  bird_cols <- dplyr::select(NSW_water_sources_HEVAE_Distinct_agg, 
                             contains("bird")) %>% names() %>% .[-1]
  
  veg_cols <- dplyr::select(NSW_water_sources_HEVAE_Distinct_agg, 
                            contains("veg")) %>% names() %>% .[-1]
  
  other_cols <- setdiff(names(NSW_water_sources_HEVAE_Distinct_agg),
                        c(frog_cols, fish_cols, bird_cols, veg_cols)) %>% 
    sort() %>% .[-1] %>% .[seq(from = 1, to = length(.), by = 4)]
  
  
  ## Make a list of column names
  col_list <- list()
  for(spp_code in other_cols) {
    
    ## spp_code <- other_cols[1]
    spp_col  <- sub("\\_.*", "", spp_code, "")
    
    spp_cols <- dplyr::select(NSW_water_sources_HEVAE_Distinct_agg, 
                              contains(spp_col)) 
    
    message('assigning ', spp_col)
    col_list[[spp_code]] <- spp_cols
    
    # new_name <- paste0("filtered_xfile",i)
    # 
    # # Store object 'data' under the name 'new_name' in the global environment.
    # assign(new_name, data)
    
  }
  
}



## 
# NSW_water_sources_HEVAE_Distinct_codes <- NSW_water_sources_HEVAE_Distinct_agg %>% 
#   
#   ##
#   mutate(AApl_men = rowMeans(select(all_of(col_list[1])), na.rm = TRUE))




#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################