################################### ---- LG report LIST FUNCTIONS ---- ######################################


## Functions to create report lists for LG21


## Auto fit a flexable to the page ----
FitFlextableToPage <- function(ft, pgwidth = 6){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
  
}



# report_data       = Output_1_water_sources_distinct
# report_dir        = report_dir
# report_template   = paste0(Sys.getenv('source_control_local_path'),
#                             'Main/R Scripts - Production/Supply calculations/LG_report list_template.docx')
# 
# event              = event_group_ID
# 
# EMU_cols            = c("Item",
#                        "Received",
#                        "Specifics",
#                        "Quantity")
# 
# ## Table variables
# font_size      = 9
# head_size      = 12
# column_col     = "wheat"
# title_col      = "#779ecb"
# header_col     = "black"
# border_col     = "darkgrey"
# dash_col       = "black"
# 
# bg_col         = '#FFFFFF'
# highlight_col  = "lightgrey"





## Create flextables, word docs and excel files for all EMUs ----
create_CHR_EMU_report_table_lists <- function(report_data,       report_dir, 
                                              report_template,   event,
                                              EMU_cols,           head_size,   font_size,  
                                              column_col,        title_col,   header_col,
                                              border_col,        dash_col,    bg_col,      
                                              highlight_col) {
  
  
  ## Pipe the list into lapply
  ## emu = sort(unique(report_data$DeliveryLocation))[[1]]
  lapply(unique(report_data$DeliveryLocation), function(emu) {
    
    ## First, create a directory for the emu that is the full path
    emu_dir = paste0(report_dir, '/', emu)
    
    ## If the directory doesn't exist, create it and run the analyses
    if(!dir.exists(emu_dir) == TRUE) {
      message('Creating directory for ', emu)
      dir.create(emu_dir)
      
      ## Check if the report list already exists
      EMU_list <- paste0(emu_dir, '/', emu, ' report list.docx')
      if(file.exists(EMU_list) == FALSE) {
        message("Creating report list table for ", emu)
        
        ## Create a title to be used for display and saving
        Date                = lubridate::now()
        EMU_title     = paste0(emu, ' report List ', event) %>% gsub('EMU', 'Returning', .)
        EMU_Pre_emu_title = paste0(emu, ' : Pre-poll report List ', event) %>% gsub('EMU', 'Returning', .)
        
        ## Get unique values of the columns we don't want to repeat in list table
        Location_EMU <- 'Returning Office'
        
        ## Heading and time are constant, plus the unique number of water_source types for each EMU
        header_positon = length(c(EMU_emu_title, 
                                  Location_EMU,
                                  paste('Created ', Date))) + 1
        
        ## Set FT objects outside the loop
        big_b <- fp_border(color = border_col, width = 3)
        std_b <- fp_border(color = dash_col,   style = "solid", width = 0.5)
        
        ## Get the table of all water_sources that report to that EMU
        emu_table     <- filter(report_data, DeliveryLocation == emu)
        emu_pre_table <- filter(report_data, DeliveryLocation == emu & 
                                     VenueName == emu &
                                     LocationTypeCode == 'Pre-polling Place')
        
        ## Create a dataframe with the needed columns
        EMU_items <- emu_table %>%
          
          ## Create "received" column
          mutate(Received = 'o') %>%   
          rename(Quantity = Qty) %>% 
          
          ## Do we need to group by Item, and sum across all the water_sources for that emu?
          ## Process is hierarchical delivery?
          group_by(Item, Received, Specifics) %>% 
          dplyr::summarise(Quantity = sum(Quantity, na.rm = TRUE)) %>% 
          
          ## The EMU's should line up their boxes in logical order. 
          ## The table should engineer this by logical sorting. 
          arrange(Item) %>% as.data.frame(stringsAsFactors = FALSE) %>%  
          
          ## Create the flextable and add header
          select(., one_of(EMU_cols))            %>% 
          flextable()                           %>%
          
          ## Add a header for every table item that doesn't repeat
          add_header_lines(., values = rev(c(EMU_emu_title, 
                                             Location_EMU,
                                             paste('Created ', Date))), top = TRUE) %>% 
          
          ## Try autofitting and reducing font size
          ## Use Zebra theme to mimic conditional formatting
          theme_zebra() %>% 
          
          ## Align the text
          align(., j = 1, align = "left", part = "all")  %>%
          align(., j = 2, align = "left", part = "all")  %>%
          align(., j = 3, align = "left", part = "all")  %>%
          align(., j = 4, align = "left", part = "all")  %>%
          
          fontsize(., size = font_size, part = "all")    %>% 
          fontsize(., size = head_size, part = "header") %>%  
          
          ## Change cell-widths
          ## Can these be auto-fit?
          bg(.,                  bg    = column_col,    part = "header") %>%
          bg(.,    i = 1, j = 1, bg    = title_col,     part = "header") %>%
          color(., i = 1, j = 1, color = bg_col,        part = "header") %>%
          
          bg(.,    i = header_positon,  bg    = header_col,    part = "header") %>%
          color(., i = header_positon,  color = bg_col,        part = "header") %>%
          bg(.,    j = "Item",          bg    = highlight_col, part = "body")   %>%
          
          ## Create outside lines
          vline(.,        border = std_b, part = "all") %>%
          hline(.,        border = std_b)               %>%
          
          ## Make a checkbox
          font(j = 2, fontname = "Wingdings") %>% 
          set_table_properties(layout = "autofit")
        
        
        ## Create a word document for each EMU emu
        emu_name   <- gsub(' ', '_', emu)
        emu_export <- read_docx(path = report_template) %>%
          body_add_flextable(EMU_emu_items, align = "left")
        print(emu_export, paste0(emu_dir, '/', emu_name, '.docx'))
        
        ## Create a dataframe from the flextable
        emu_data      <- EMU_items$body$dataset
        emu_sheetname <- gsub(' Region EMU Office', ' EMU', emu)
        emu_sheetname <- gsub(' EMU Office', ' EMU', emu_sheetname)
        
        ## Now also save an excel spreadsheet
        delivery_workbook <- createWorkbook()
        addWorksheet(delivery_workbook, emu_sheetname)
        
        ## Freeze Panes
        freezePane(wb       = delivery_workbook, 
                   sheet    = emu_sheetname, 
                   firstRow = TRUE)
        
        ## Write the data to the corresponding worksheet
        writeDataTable(wb         = delivery_workbook, 
                       sheet      = emu_sheetname, 
                       x          = emu_data,
                       tableStyle = "TableStyleMedium16")
        
        ## Now save the workbook
        saveWorkbook(delivery_workbook, 
                     paste0(emu_dir, '/', emu, ' report list.xlsx'),
                     overwrite = TRUE)
        
        ## area = unique(emu_pre_table$LGAreaCode)[2]
        for(area in unique(emu_pre_table$LGAreaCode)) {
          
          ## Filter the EMU to the unique Area
          emu_pre_table_area <- emu_pre_table %>% filter(LGAreaCode == area)
          
          ## Get unique values of the columns we don't want to repeat in list table
          Location_EMU_pre <- emu_pre_table_area %>% 
            .$LocationTypeCode %>% unique() %>% gsub("Pre-polling Place", "Pre-poll", .)
          
          Areas_EMU        <- paste0('LGA : ', unique(emu_pre_table_area$LGAreaCode))
          
          ## Heading and time are constant, plus the unique number of water_source types for each EMU
          header_positon_pre = length(c(EMU_emu_title, 
                                        Location_EMU_pre,
                                        Areas_EMU,
                                        paste('Created ', Date))) + 1
          
          ## Create a dataframe with the needed columns
          EMU_Pre_Poll_items_area <- emu_pre_table_area %>%
            
            ## Create "received" column
            mutate(Received = 'o') %>%   
            rename(Quantity = Qty) %>% 
            
            ## Do we need to group by Item, and sum across all the water_sources for that emu?
            ## Process is hierarchical delivery?
            group_by(Item, Received, Specifics) %>% 
            dplyr::summarise(Quantity = sum(Quantity, na.rm = TRUE)) %>% 
            
            ## The EMU's should line up their boxes in logical order. 
            ## The table should engineer this by logical sorting. 
            arrange(Item) %>% as.data.frame(stringsAsFactors = FALSE) %>%  
            
            ## Create the flextable and add header
            select(., one_of(EMU_cols))            %>% 
            flextable()                           %>%
            
            ## Add a header for every table item that doesn't repeat
            add_header_lines(., values = rev(c(EMU_Pre_emu_title, 
                                               Location_EMU_pre,
                                               Areas_EMU,
                                               paste('Created ', Date))), top = TRUE) %>% 
            
            ## Try autofitting and reducing font size
            ## Use Zebra theme to mimic conditional formatting
            theme_zebra() %>% 
            
            ## Align the text
            align(., j = 1, align = "left", part = "all")  %>%
            align(., j = 2, align = "left", part = "all")  %>%
            align(., j = 3, align = "left", part = "all")  %>%
            align(., j = 4, align = "left", part = "all")  %>%
            
            fontsize(., size = font_size, part = "all")    %>% 
            fontsize(., size = head_size, part = "header") %>%  
            
            ## Change cell-widths
            ## Can these be auto-fit?
            bg(.,                  bg    = column_col,    part = "header") %>%
            bg(.,    i = 1, j = 1, bg    = title_col,     part = "header") %>%
            color(., i = 1, j = 1, color = bg_col,        part = "header") %>%
            
            bg(.,    i = header_positon_pre,  bg    = header_col,    part = "header") %>%
            color(., i = header_positon_pre,  color = bg_col,        part = "header") %>%
            bg(.,    j = "Item",              bg    = highlight_col, part = "body")   %>%
            
            ## Create outside lines
            vline(.,        border = std_b, part = "all") %>%
            hline(.,        border = std_b)               %>%
            
            ## Make a checkbox
            font(j = 2, fontname = "Wingdings") %>% 
            set_table_properties(layout = "autofit")
          
          ## And for each EMU emu : Polling Place
          emu_name            <- gsub(' ', '_', emu)
          Location_EMU_pre_name   <- gsub(' ', '_', Location_EMU_pre)
          area_name              <- gsub(' ', '_', area)
          
          emu_export_pre_area <- read_docx(path = report_template) %>%
            body_add_flextable(EMU_emu_Pre_Poll_items_area, align = "left")
          
          print(emu_export_pre_area, paste0(emu_dir, '/', area_name, '_', 
                                               Location_EMU_pre_name, '_', emu_name, '.docx'))
          
        }
        
      } else {
        message('EMU list already exists for ', emu, ' skip report lists')
      }
      
      ## If the EMU emu folder has already been created, skip the report list creation
    } else {
      message('Directory already exists for ', emu, ' skip report lists')
    }
    
  })
}


# report_data       = Output_1_water_sources_distinct
# report_dir        = report_dir
# report_template   = paste0(Sys.getenv('source_control_local_path'), ## Word document template to add formatting
#                             'Main/R Scripts - Production/Supply calculations/LG_report list_template.docx')
# 
# water_source_cols = c("Item",
#                "Type",
#                "Received",
#                "Packed",
#                "Specifics",
#                "Quantity")
# 
# ## Table variables
# font_size      = 9
# head_size      = 12
# column_col     = "wheat"
# title_col      = "#779ecb"
# header_col     = "black"
# border_col     = "darkgrey"
# dash_col       = "black"
# 
# bg_col         = '#FFFFFF'
# highlight_col  = "lightgrey"


## Create flextables, word docs and excel files for all Water Sources ----
create_CHR_WS_report_lists <- function(report_data,  report_dir, report_template, event,
                                       water_source_cols,    head_size,   font_size,  
                                       column_col,    title_col,   header_col,
                                       border_col,    dash_col,    bg_col,      highlight_col) {
  
  
  ## First, get rid of the EMU wss that don't have Venues attached - EG the Sydney Count Centre
  report_data <- report_data[!is.na(report_data$VenueName), ]
  
  ## Pipe the list into lapply
  ## ws = unique(report_data$DeliveryLocation)[1]
  lapply(unique(report_data$DeliveryLocation), function(ws) {
    
    ## First, create a directory for the ws that is the full path
    ws_dir = paste0(report_dir, '/', ws)
    
    ## Create the directory if it doesn't exist
    if(dir.exists(ws_dir) == TRUE) {
      
      ## Get the area name of the EMU ws
      main_area_name <- gsub(' Region EMU Office', '', ws)
      
      ## Get the table of all water_sources that report to that EMU
      ## But, exclude the EMU Office itself - that has been done to the Pre-poll in the EMU function
      ws_table <- filter(report_data, DeliveryLocation == ws) %>% 
        filter(str_detect(LocationTypeCode, "Returning Office", negate = TRUE)) %>% 
        filter(str_detect(VenueName,        "EMU Office",        negate = TRUE))
      
      ## Loop over all the Venues for that EMU
      ## water_source = unique(ws_table$VenueName)[28]
      lapply(unique(ws_table$VenueName), function(water_source) {
        
        ## Check if the report list already exists
        # Venue_list <- paste0(ws_dir, '/', area_name, '_', Location_Venue_pre, '_', water_source_name, '.docx')
        # if(file.exists(Venue_list) == FALSE) {
        message("Creating report list table for ", water_source)
        
        ## Create a title to be used for display and saving
        Date         = lubridate::now()
        ws_title = paste0(ws, ' report List')
        water_source_title  = paste0(water_source,  ' report List')
        
        ## Get unique values of the columns we don't want to repeat in the list table
        Delivery <- filter(ws_table, VenueName == water_source) %>% 
          .$DeliveryLocation %>% unique()
        
        Location <- filter(ws_table, VenueName == water_source) %>% 
          .$LocationTypeCode %>% unique()
        
        ## Add a length to position the black text
        header_positon = length(c(water_source_title, 
                                  Delivery, 
                                  Location, 
                                  paste('Created ', Date))) + 1
        
        Venue <- filter(ws_table, VenueName == water_source) %>% 
          .$VenueName %>% unique()
        
        ## Set FT objects outside the loop
        big_b <- fp_border(color = border_col, width = 3)
        std_b <- fp_border(color = dash_col,   style = "solid", width = 0.5)
        
        ## Create Venue Table
        water_source_table <- filter(ws_table, VenueName == water_source) %>% 
          mutate(LGAreaCode = ifelse(is.na(LGAreaCode), main_area_name, LGAreaCode))
        
        ##
        for(area in unique(water_source_table$LGAreaCode)) {
          
          ## Filter the Venue to the unique Area
          # area = unique(water_source_table$LGAreaCode)[1]
          ws_pre_table_area <- water_source_table %>% filter(LGAreaCode == area)
          
          if(nrow(ws_pre_table_area) > 0) {
            
            ## Get unique values of the columns we don't want to repeat in list table
            Location_Venue_pre <- ws_pre_table_area %>% 
              .$LocationTypeCode %>% unique() %>% gsub("Pre-polling Place", "Pre-poll", .)
            
            Areas_Venue        <- paste0('LGA : ', unique(ws_pre_table_area$LGAreaCode))
            
            ## Heading and time are constant, plus the unique number of water_source types for each Venue
            header_positon_pre = length(c(water_source_title, 
                                          Location_Venue_pre,
                                          Areas_Venue,
                                          paste('Created ', Date))) + 1
            
            ## Create a dataframe with the needed columns
            Venue_ws_Pre_Poll_items_area <- ws_pre_table_area %>%
              
              ## Create "received" column
              mutate(Received = 'o') %>%
              mutate(Packed   = 'o') %>%  
              rename(Quantity = Qty) %>% 
              
              ## Do we need to gVenueup by Item, and sum across all the water_sources for that ws?
              ## Process is hierarchical delivery?
              group_by(Item, Received, Packed, Specifics) %>% 
              dplyr::summarise(Quantity = sum(Quantity, na.rm = TRUE)) %>% 
              
              ## The EMU's should line up their boxes in logical order. 
              ## The table should engineer this by logical sorting. 
              arrange(Item) %>% as.data.frame(stringsAsFactors = FALSE) %>%  
              
              ## Create the flextable and add header
              select(., one_of(water_source_cols)) %>% 
              flextable()                   %>%
              
              ## Add a header for every table item that doesn't repeat
              add_header_lines(., values = rev(c(water_source_title, 
                                                 Location_Venue_pre,
                                                 Areas_Venue,
                                                 paste('Created ', Date))), top = TRUE) %>% 
              
              ## Try autofitting and reducing font size
              ## Use Zebra theme to mimic conditional formatting
              theme_zebra() %>% 
              
              ## Align the text
              align(., j = 1, align = "left", part = "all")  %>%
              align(., j = 2, align = "left", part = "all")  %>%
              align(., j = 3, align = "left", part = "all")  %>%
              align(., j = 4, align = "left", part = "all")  %>%
              
              fontsize(., size = font_size, part = "all")    %>% 
              fontsize(., size = head_size, part = "header") %>%  
              
              ## Change cell-widths
              ## Can these be auto-fit?
              bg(.,                  bg    = column_col,    part = "header") %>%
              bg(.,    i = 1, j = 1, bg    = title_col,     part = "header") %>%
              color(., i = 1, j = 1, color = bg_col,        part = "header") %>%
              
              bg(.,    i = header_positon_pre,  bg    = header_col,    part = "header") %>%
              color(., i = header_positon_pre,  color = bg_col,        part = "header") %>%
              bg(.,    j = "Item",              bg    = highlight_col, part = "body")   %>%
              
              ## Create outside lines
              vline(.,        border = std_b, part = "all") %>%
              hline(.,        border = std_b)               %>%
              
              ## Make a checkbox
              font(j = 2, fontname = "Wingdings") %>% 
              font(j = 3, fontname = "Wingdings") %>% 
              set_table_properties(layout = "autofit")
            
            ## And for each EMU ws : Polling Place
            water_source_name           <- gsub(' ', '_', water_source)
            Location_Venue_pre   <- gsub(' ', '_', Location_Venue_pre)
            area_name            <- gsub(' ', '_', area)
            
            water_source_export_pre_area <- read_docx(path = report_template) %>%
              body_add_flextable(Venue_ws_Pre_Poll_items_area, align = "left")
            print(water_source_export_pre_area, paste0(ws_dir, '/', area_name, '_', Location_Venue_pre, '_', water_source_name, '.docx'))
            
          } else {
            message('NA LGArea for ', water_source, ' skip report lists')
          }
          
        }

      })
    } else {
      message('Directory does not exist for ', ws, ' Skip')
    }
  })
}





######################################## -------- TBC --------- ###########################################