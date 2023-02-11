############################################################################################
############################ ----  STATS FUNCTIONS ---- ####################################
############################################################################################


## NON-LINEAR MODELS ==========================================================================


# yvars  = c('pc10th ~ ',
#            'pc20th ~ ',
#            'pc30th ~ ',
#            'pc40th ~ ',
#            'pc50th ~ ',
#            
#            'pc60th ~ ',
#            'pc65th ~ ',
#            'pc70th ~ ',
#            'pc75th ~ ',
#            
#            'pc80th ~ ',
#            'pc82nd ~ ',
#            'pc84th ~ ',
#            'pc86th ~ ',
#            'pc88th ~ ',
#            
#            'pc90th ~ ',
#            'pc92nd ~ ',
#            'pc94th ~ ',
#            'pc96th ~ ',
#            'pc98th ~ ',
#            'pc100th ~ ')
# 
# lim    = 10 
# 
# xvars  = c('s(min.BDW,  k=3) +
#            s(Harc_Stream_density_sqkm, k=3)')
# 
# model_data     = HARC_flow_measures_10
# sel_method     = 'REML'
# predict_data   = CHR_sources_zonal_stats_table_eligible
# out_dir        = paste0(hydro_out, 'harc_models/')




## Create a list of GAM analyses for each topic ----
gam_formula_analyses <- function(yvars, 
                                 xvars,
                                 lim,
                                 model_data,
                                 sel_method,
                                 predict_data,
                                 out_dir) {
  
  ## Get predict data
  model_chr         <- model_data$CHR_Water_Source %>% unique()
  predict_data_test <- predict_data #%>% filter(!CHR_Water_Source %in% model_chr) 
  
  ## Pipe the list into Lapply
  gam_list <- yvars %>%
    
    ## Pipe the list into lapply
    ## yvar <-  yvars[1]
    lapply(function(yvar) {
      
      # if(!all(is.na(analysis_data[[exp_var]]))) {
      
      ## Step 1: Define GWR bandwidth
      message('running GAM for ', lim, ' limit, ', yvar)
      GAM_formula <- paste0(yvar, xvars)
      
      GAM_model   <- gam(as.Formula(GAM_formula), 
                         
                         method = sel_method, 
                         data   = model_data)
      
      GAM_summary <- summary(GAM_model)
      GAM_table   <- as_flextable(GAM_model)
      GAM_DE      <- GAM_summary$dev.expl %>% round(., 3) %>% "*"(100)
      
      ## Plots
      GAM_plot <- 
        gratia::draw(GAM_model, 
                     residuals = TRUE, 
                     cex       = 3,
                     color     = 'red') & 
        theme_bw() & ggtitle(paste0(yvar, lim, ' ', GAM_DE, ' % DE')) 
      
      GAM_appr   <- appraise(GAM_model) & 
        theme_bw() & ggtitle(paste0(yvar, lim)) 
      
      ## Save the plots
      png(paste0(out_dir, yvar, lim, '_gam_effect_plot.png'),
          10, 6, units = 'in', res = 300)
      print(GAM_plot)
      dev.off()
      
      png(paste0(out_dir, yvar, lim, '_gam_residual_plot.png'),
          10, 6, units = 'in', res = 300)
      print(GAM_appr)
      dev.off()
      
      
      ## Predictions of other water sources
      GAM_predict_flow  <- mgcv::predict.gam(GAM_model, predict_data_test) %>% as_tibble()
      GAM_predict_terms <- mgcv::predict.gam(GAM_model, predict_data_test, type = "terms") %>% as_tibble()
      GAM_predict_exc   <- predict(GAM_model, predict_data_test, exclude = "s(x0)", 
                                   newdata.guaranteed = TRUE) %>% as_tibble()
      GAM_predict_mat   <- predict(GAM_model, predict_data_test, type = "lpmatrix") %>% as_tibble()
      
      ## Save Harc predictions on themselves
      yvar_pred <- paste0(yvar, '_gam') %>% str_replace_all(., " ~ ", "")
      yvar_col  <- str_replace_all(yvar, " ~ ", "")
      
      GAM_predict_flow_harc  <- mgcv::predict.gam(GAM_model, model_data) %>% as_tibble()
      # GAM_predict_flow_harc$Harvest_limit <- lim
      names(GAM_predict_flow_harc) <- c(yvar_pred) 
      
      # model_vars_out <- add_column(model_vars, 
      #                              !!yvar_pred := GAM_predict_flow_harc$value, 
      #                              .after = yvar_col) %>% 
      #   as_tibble()
      # 
      # write_csv(model_vars_out, paste0(out_dir, yvar_col, '_', lim, 'lim_HARC_GAM_predict.csv'))
      
      ## Predictions with Confidence Intervals
      GAM_predict_CI    <- predict(GAM_model, predict_data_test, type = "link", se.fit = TRUE) %>% 
        
        as_tibble() %>% 
        mutate(CHR_Water_Source = predict_data_test$CHR_Water_Source,
               Upper_CI          = fit + (2  * se.fit),
               Lower_CI          = fit + (-2 * se.fit),
               Harvest_limit     = lim) %>% 
        select(CHR_Water_Source, Harvest_limit, fit, se.fit, Lower_CI, Upper_CI) 
      
      colnames(GAM_predict_CI)[3]   <- paste(colnames(GAM_predict_CI)[3],   yvar, sep = "_")
      colnames(GAM_predict_CI)[4:6] <- paste(colnames(GAM_predict_CI)[4:6], yvar, sep = "_")
      
      
      GAM_results <- list(GAM_model,        GAM_summary,    GAM_table, 
                          GAM_plot,         GAM_appr,       GAM_predict_flow_harc,
                          GAM_predict_flow, GAM_predict_CI, GAM_predict_terms, GAM_predict_mat)
      
      names(GAM_results) <- c(paste0(yvar, '_GAM_model'), 
                              paste0(yvar, '_GAM_summary'),
                              paste0(yvar, '_GAM_table'),
                              paste0(yvar, '_GAM_plot'),
                              paste0(yvar, '_GAM_resid'),
                              paste0(yvar, '_GAM_predict_harc'),
                              paste0(yvar, '_GAM_predict_flow'),
                              paste0(yvar, '_GAM_predict_CI'),
                              paste0(yvar, '_GAM_predict_terms'),
                              paste0(yvar, '_GAM_predict_mat'))
      
      return(GAM_results)
      
      # } else {
      #   message('Skip GAM for ', formula, ' insufficient data for this question')
      #   cat(formula)
      # }
      
    })
  
  ## Rename the list items
  names(gam_list) <- yvars
  names(gam_list) <- paste0("GAM : ", names(gam_list))
  return(gam_list)
  
}





###########################################################################################
############################################ ---TBC---- ####################################
############################################################################################