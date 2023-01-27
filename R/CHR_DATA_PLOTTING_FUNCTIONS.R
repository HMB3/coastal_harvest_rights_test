############################################################################################
############################ ----  PLOTTING FUNCTIONS ---- #################################
############################################################################################


## PLOT WRANGLING ==========================================================================


## Create Qualitative colour scale ----
## https://github.com/etiennebr/visualraster
visras_scale_fill_discrete = function(values = brewer.pal(11, "Set3"), ...) {
  scale_fill_manual(values = values, ...)
}


## Number of ticks ----
## A function to transform the numbers on the axes to be 1 significanty didgit 
number_ticks <- function(n) {function(limits) pretty(limits, n)}


## Only display every nth y-axis value
every_nth = function(n) {
  
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  
}





## BAR CHARTS ==============================================================================


# df    = Q4_plot
# title = ''
# caption  = paste0(role_list[['Q4_Roles']],
#                   " = ",  count_list[['Q4_Count']])
# 
# tsize       = ''
# capt_size   = 20
# xsize       = 20
# ysize       = 20
# ycol        = 'black'
# lab_size    = 8
# col_palette = "Set2"
# 
# ymin  = 0
# ymax = max(df$Risk) + max(df$Risk*0.2)
# ylab  = ylab
# xlab  = ''

## Bar chart horizontal ---- 
single_barchart_order_y = function(df, title, xvar, yvar, ylab, xlab, col, 
                                   # ymin, ymax, 
                                   # axis_multiplier, 
                                   mar,
                                   tsize, caption, xsize, ysize, ycol, 
                                   lab_size, bar_width, capt_size, color_n) {
  
  plot <- ggplot(df, aes(x = reorder(!!sym(xvar), !!sym(yvar)),
                         y = !!sym(yvar), fill = !!sym(yvar))) +
    geom_bar(stat = "identity", position = "dodge", fill = col) +
    coord_flip() +
    
    # scale_fill_manual(values = as.vector(watlington(color_n))) +
    
    # geom_text(aes(label = !!sym(xvar), hjust = + 0.5), 
    #           hjust = -0.5, 
    #           position = position_dodge(width = 1),
    #           inherit.aes = TRUE,
    #           size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    # scale_y_continuous(labels = scales::percent, 
    #                    limits = c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  return(plot)
  
}





## Create a single bar chart un-ordered ---- 
single_bar_order_factor = function(df, title, ylab, xlab, ymin, ymax, axis_multiplier, col_palette,
                                   tsize, caption, xsize, ysize, ycol, 
                                   lab_size, bar_width, capt_size) {
  
  ## Set the max and min inside function
  ymax = max(df$Risk) + max(df$Risk * axis_multiplier)
  
  plot <- ggplot(df, aes(x = Response, y = Risk, fill = Class)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_colour_brewer(palette = col_palette, na.value = "grey") +
    
    geom_text(aes(label = Risk, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1,1,1,1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  return(plot)
  
}





## Create a single bar chart facet ---- 
single_bar_order_factor_facet = function(df, title, Response, ylab, xlab, ymin, ymax, axis_multiplier, col_palette,
                                         tsize, strip_size, caption, xsize, ysize, ycol, mar,
                                         lab_size, bar_width, capt_size, facet_var, wrap_scale) {
  
  ## Set the max and min inside function
  ymax = max(df$Risk) + max(df$Risk * axis_multiplier)
  
  plot <- ggplot(df, aes(x = !!sym(Response), y = Risk, fill = !!sym(Response))) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_colour_brewer(palette = col_palette, na.value = "grey") +
    
    geom_text(aes(label = Risk, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          strip.text      = element_text(size  = strip_size, face = 'bold'),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black")) +
    
    facet_wrap(as.formula(paste("~", facet_var)), scales = wrap_scale)
  
  return(plot)
  
}





## single_barchart_order_y_small ---- 
single_barchart_order_y_small = function(df, title, ylab, xlab, ymin, ymax, mar,
                                         tsize, caption, xsize, ysize, ycol, colours,
                                         lab_size, bar_width, capt_size) {
  
  plot <- ggplot(df, aes(x = Response, y = Risk, fill = Class)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_fill_manual(values = colours, na.value = "grey") +
    
    geom_text(aes(label = Risk, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    
    scale_y_continuous(labels = scales::percent, limits=c(ymin,ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  return(plot)
  
}



## Bar chart horizontally stacked, ordered by date ----
horizontal_bar_two_factor_facet = function(df, title, xvar, yvar, group_var, fill_var, lab_angle, colours,
                                           ylab, xlab, ymin, ymax, leg_pos, date_break, x_int,
                                           tsize, caption, xsize, ysize, xtitle, ytitle, strip_size, ycol, h_just,
                                           lab_size, capt_size, leg_size, axis_size, plot_margin, facet_var,
                                           wrap_scale) {
  
  ## Use variable names for x and y
  ggplot(df,
         aes(
           x = !!sym(xvar),
           y = !!sym(yvar), 
           fill = !!sym(group_var)
           #, colour = !!sym(group_var)
         )) +
    
    geom_bar(stat     = "identity",  
             #width = 0.4, 
             position = 'dodge') +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    ## Control the date
    #scale_x_discrete(breaks = every_nth(n = x_int)) +
    #scale_x_date(date_labels = "%d-%m", date_breaks = date_break) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    #ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(plot_margin, plot_margin, plot_margin, plot_margin), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          strip.text      = element_text(size  = strip_size, face = 'bold'),
          axis.text.x     = element_text(size = xsize,      angle = lab_angle, vjust =-0.02),
          axis.title.x    = element_text(size   = xtitle,   face = "bold",     vjust =-5),
          axis.line       = element_line(colour = 'black', size = axis_size),
          
          legend.position = leg_pos,
          legend.text     = element_text(size = leg_size),
          legend.title    = element_blank(),
          
          axis.title.y    = element_text(size = ytitle,  face = "bold", vjust = 4),
          axis.text.y     = element_text(size = ysize, color = ycol),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black")) +
    
    facet_wrap(as.formula(paste("~", facet_var)), scales = wrap_scale)
  
  
}





## Bar chart horozontally stacked, few categories ----
dash_bar_chart_small = function(df, title, xvar, yvar, fill_var, colours,
                                ylab, xlab, ymin, ymax,
                                tsize, caption, xsize, ysize, ycol,
                                lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(
                   x    = reorder(!!sym(xvar), !!sym(yvar)),
                   y    = !!sym(yvar),
                   fill = !!sym(xvar))) +
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_fill_manual(values = colours, na.value = "grey") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart vertically stacked and ordered by value ----
dash_bar_chart_yvar = function(df, title, xvar, yvar,
                               ylab, xlab, ymin, ymax,
                               tsize, caption, xsize, ysize, ycol,
                               lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
                     y    = !!sym(yvar),
                     fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}



## Bar chart vertically stacked, ordered by value, no x-axis ----
dash_bar_chart_xblank = function(df, title, xvar, yvar,
                                 ylab, xlab, ymin, ymax,
                                 tsize, caption, xsize, ysize, ycol,
                                 lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = reorder(!!sym(xvar), !!sym(yvar)),
                     y    = !!sym(yvar),
                     fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart vertically stacked and ordered by factor ----
dash_bar_chart_xvar = function(df, title, xvar, yvar,
                               ylab, xlab, ymin, ymax,
                               tsize, caption, xsize, ysize, ycol,
                               lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  plot <- ggplot(df,
                 aes(x    = !!sym(xvar), !!sym(yvar),
                     y    = !!sym(yvar),
                     fill = !!sym(xvar))) +
    
    geom_bar(stat = "identity") +
    
    coord_flip() +
    scale_color_brewer(palette = "Dark2") +
    
    geom_text(aes(label = !!sym(yvar), hjust = + 0.5),
              hjust     = -0.5,
              position  = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(0.5,0.5,0.5,0.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold", vjust=-5),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face  = "bold",   vjust=-5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Bar chart horizontal with factor ---- 
dash_bar_chart_factor = function(df, title, xvar, yvar, factor, width, colours,
                                 ylab, xlab, ymin, ymax,
                                 tsize, caption, xsize, ysize, ycol, 
                                 lab_size, hjust, vjust, lab_angle, leg_size,
                                 bar_width, capt_size) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, 
                 aes(
                   x = reorder(!!sym(xvar), !!sym(xvar)),
                   y = !!sym(yvar), 
                   fill = factor(!!sym(factor)))) +  
    geom_bar(stat = "identity", position = "stack", width = width) + 
    
    scale_fill_manual(values = colours, na.value = "grey") +
    
    geom_text(aes(label = !!sym(yvar), hjust = hjust), 
              hjust = 0.6, vjust = vjust, 
              position = position_dodge(width = 0),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1,1,1,1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size = xsize, face = "bold", hjust = 2),
          legend.position = 'bottom',
          legend.title    = element_blank(),
          legend.text     = element_text(size = leg_size),
          
          axis.line       = element_line(colour = 'black', size = width),
          axis.ticks      = element_line(colour = "black", size = width),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 5),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}



## Plot histogram ----
plot_histogram = function(df,   title, subtitle,  xvar, med,
                          bin,  col_var, 
                          ylab, xlab, ymin, ymax, 
                          tsize, caption, xsize, ysize, 
                          lab_size, bar_width, capt_size) {
  
  
  ## Use the 'SOURCE' column to create a histogram for each source.
  plot <- ggplot(df, 
                 aes(!!sym(xvar), 
                     fill  = !!sym(col_var), 
                     color = !!sym(col_var))) +
    
    geom_histogram(position = "identity",
                   alpha    = 0.3,
                   binwidth = bin,
                   color="black") +
    
    ## add lables 
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption  = caption,
         subtitle = subtitle) +
    
    ylim(c(ymin, ymax)) +
    
    geom_vline(xintercept = med, color = "red", linetype = "solid", size = 1) +
    geom_text(aes(x = med, label = paste(med), y = ymax), colour = "blue", angle = 0, size = 10) +
    
    
    theme(plot.margin     = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -1.5),
          legend.position = 'none',
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, vjust = -3.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Plot time histogram ----
plot_time_histogram = function(df,   title, subtitle,  xvar, med,
                               bin,  col_var, median_pos,
                               ylab, xlab,  
                               tsize, caption, xsize, ysize, 
                               lab_size, bar_width, capt_size) {
  
  # Function for creating a sequence of breaks
  by_minutes <- function(x , n = 1){
    
    seq(min(x,na.rm=T)
        ,max(x,na.rm=T)
        ,by = paste0(n, " mins"))
    
  }
  
  calculated_breaks <- by_minutes(df[[xvar]], bin) # bin argument refers to bin width in minutes
  # med_lab <- format(med, "%H:%M:%S") %>% as.POSIXct()
  
  
  ## Use the 'SOURCE' column to create a histogram for each source.
  plot <- ggplot(df, 
                 aes(!!sym(xvar))) +
    
    geom_histogram(position = "identity",
                   alpha    = 0.3,
                   breaks = calculated_breaks,
                   color = "black") +
    
    ## add labels 
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption  = caption,
         subtitle = subtitle) +
    
    ## Add a median line
    ## The x-scale here is wrong, but not sure how to fix it...
    geom_vline(xintercept = med, color = "red", linetype = "solid", size = 1) +
    geom_text(aes(x = med, label = paste(med), y = median_pos), colour = "blue", angle = 0, size = 10) +
    
    scale_x_datetime(date_breaks = "1 hour"
                     ,date_labels = "%H:%M") +
    
    ## Set themes
    theme(plot.margin     = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -1.5),
          legend.position = 'none',
          legend.title    = element_blank(),
          
          axis.title.y    = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y     = element_text(size = ysize),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5,  face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, vjust = -3.5, face = "italic", color = "black"))
  
  ## Scale the x-axis here so that it handles the time format
  return(plot)
  
}





## Grouped Boxplots ---- 
factor_boxplots_facet = function(df, x_var, y_var,     pallette, group_var,
                                 box_size,  x_lab,     y_lab, y_lim, leg_pos,
                                 lab_size,  lab_angle, border_size, facet_var, wrap_scale) {
  
  plot <- ggboxplot(df, 
                    x       = x_var, 
                    y       = y_var, 
                    fill    = group_var,
                    palette = pallette, size = box_size) +
    
    geom_hline(yintercept = 0, col = 'black', linetype = "dashed") +
    
    ## Use the classic theme
    theme_classic(base_size = 16) +
    labs(y = y_lab,
         x = '') +
    scale_y_continuous(limits = y_lim) +
    geom_abline(intercept = 0, slope = 0, size = 1, color = 'grey', linetype = 'dashed') +
    
    ## Change the axes sizes, etc.
    theme(axis.title.x     = element_text(colour = 'black',  size  = lab_size),
          # axis.text.x      = element_text(size   = lab_size, angle = lab_angle, 
          #                                 face   = "bold",   hjust = 1),
          strip.text       = element_text(size  = lab_size, face = 'bold'),
          
          axis.text.x      = element_blank(),
          
          axis.title.y     = element_text(colour = 'black',  size = lab_size, face = "bold"),
          axis.text.y      = element_text(size   = lab_size),
          
          panel.border     = element_rect(colour = 'black',  fill = NA, size = border_size),
          plot.title       = element_text(size   = lab_size, face = 'bold'),
          legend.text      = element_text(size   = lab_size, face = 'bold'),
          legend.title     = element_blank(),
          legend.position  = leg_pos,
          legend.key.size  = unit(1.5, 'cm')) +
    
    facet_wrap(as.formula(paste("~", facet_var)), scales = wrap_scale) +
    
    ## And title
    ggtitle(paste0(''))
  
  return(plot)
  
}





## LINE PLOTS ==============================================================================


## Grouped line plot ordered by y-value ----
group_line_plot_order_y = function(df,       title, xvar, yvar, line_size, point_size,
                                   ylab,     xlab, group_var, lab_angle,   leg_size, 
                                   tsize,    caption, xsize, ysize, ycol, y_break,
                                   lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  plot <- ggplot(df,
                 aes(
                   x = reorder(!!sym(xvar), !!sym(yvar)),
                   y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin   = unit(c(1, 1, 1, 1), "cm"),
          plot.title    = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x   = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x  = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text   = element_text(size  = leg_size),
          legend.title  = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Grouped line plot ordered by y-value ----
group_line_plot = function(df,       title, xvar, yvar, line_size, point_size,
                           ylab,     xlab, group_var, lab_angle,   leg_size, 
                           tsize,    caption, xsize, ysize, ycol, y_break,
                           lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  plot <- ggplot(df,
                 aes(
                   x = !!sym(xvar),
                   y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = 'bottom',
          
          axis.title.y    = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y     = element_text(size = ysize, color = ycol),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Grouped line plot no legend ----
group_line_plot_noleg = function(df,       title, xvar, yvar, line_size, point_size,
                                 ylab,     xlab, group_var, lab_angle,   leg_size, 
                                 tsize,    caption, xsize, ysize, ycol, y_break,
                                 lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  plot <- ggplot(df,
                 aes(
                   x = !!sym(xvar),
                   y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text     = element_text(size  = leg_size),
          legend.position = 'none',
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Grouped line plot both axes ----
group_line_plot_skip_x_axes = function(df,       title, xvar, yvar, line_size, point_size, x_int,
                                       ylab,     xlab, group_var, lab_angle,   leg_size, 
                                       tsize,    caption, xsize, ysize, ycol,
                                       lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  plot <- ggplot(df,
                 aes(
                   x = reorder(!!sym(xvar), !!sym(yvar)),
                   y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ## Limit the number of x axis values if the axis is cluttered,
    ## EG if there are too many dates
    scale_x_discrete(breaks = every_nth(n = x_int)) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.text     = element_text(size  = leg_size),
          legend.position = 'bottom',
          legend.title    = element_blank(),
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Grouped line plot both axes ----
group_line_plot_y_axes = function(df,       title, xvar, yvar, line_size, point_size,
                                  ylab,     xlab, group_var, lab_angle,   leg_size,
                                  tsize,    caption, xsize, ysize, ycol,
                                  lab_size, bar_width, capt_size) {
  
  ## Use variable names for x and y
  ## Replaced group = 1
  plot <- ggplot(df,
                 aes(
                   x = reorder(!!sym(xvar), !!sym(yvar)),
                   y = !!sym(yvar), group = !!sym(group_var), colour = !!sym(group_var))) +
    
    geom_line(size  = line_size) +
    geom_point(size = point_size, colour = 'grey') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_blank(),
          axis.title.x    = element_blank(),
          legend.text     = element_text(size  = leg_size),
          legend.title    = element_blank(),
          legend.position = 'bottom',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}





## Single line plot ----
single_line_plot = function(df,  title,  xvar, yvar,
                            ylab, xlab,  lab_angle, line_size, point_size,
                            tsize, caption, xsize, ysize, 
                            lab_size, bar_width, capt_size, leg_size) {
  
  ## Use variable names for x and y
  plot <- ggplot(df, aes(!!sym(xvar), !!sym(yvar)), group = 1) +
    geom_line(size = line_size, group = 1) +
    
    geom_line(size  = line_size, group = 1, colour = 'blue') +
    geom_point(size = point_size, colour = 'orange') +
    
    theme_light(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    theme(plot.margin     = unit(c(1, 1, 1, 1), "cm"),
          plot.title      = element_text(vjust = 5,     size  = tsize,     face  = "bold"),
          axis.text.x     = element_text(size  = xsize, angle = lab_angle, hjust = 1),
          axis.title.x    = element_text(size  = xsize, face  = "bold",    vjust = -5),
          legend.position = 'bottom',
          
          axis.title.y  = element_text(size = ysize, face = "bold", vjust = 4),
          axis.text.y   = element_text(size = ysize),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
  return(plot)
  
}




## SCATTER-PLOTS =======================================================================


#define a helper function (borrowed from the "ez" package)
ezLev=function(x,new_order){
  for(i in rev(new_order)){
    x=relevel(x,ref=i)
  }
  return(x)
}

ggcorplot = function(data,var_text_size,cor_text_limits){
  # normalize data
  for(i in 1:length(data)){
    data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
  }
  # obtain new data frame
  z=data.frame()
  i = 1
  j = i
  while(i<=length(data)){
    if(j>length(data)){
      i=i+1
      j=i
    }else{
      x = data[,i]
      y = data[,j]
      temp=as.data.frame(cbind(x,y))
      temp=cbind(temp,names(data)[i],names(data)[j])
      z=rbind(z,temp)
      j=j+1
    }
  }
  names(z)=c('x','y','x_lab','y_lab')
  z$x_lab = ezLev(factor(z$x_lab),names(data))
  z$y_lab = ezLev(factor(z$y_lab),names(data))
  z=z[z$x_lab!=z$y_lab,]
  #obtain correlation values
  z_cor = data.frame()
  i = 1
  j = i
  while(i<=length(data)){
    if(j>length(data)){
      i=i+1
      j=i
    }else{
      x = data[,i]
      y = data[,j]
      x_mid = min(x)+diff(range(x))/2
      y_mid = min(y)+diff(range(y))/2
      this_cor = cor(x,y)
      this_cor.test = cor.test(x,y)
      this_col = ifelse(this_cor.test$p.value<.05,'<.05','>.05')
      this_size = (this_cor)^2
      cor_text = ifelse(
        this_cor>0
        ,substr(format(c(this_cor,.123456789),digits=2)[1],2,4)
        ,paste('-',substr(format(c(this_cor,.123456789),digits=2)[1],3,5),sep='')
      )
      b=as.data.frame(cor_text)
      b=cbind(b,x_mid,y_mid,this_col,this_size,names(data)[j],names(data)[i])
      z_cor=rbind(z_cor,b)
      j=j+1
    }
  }
  names(z_cor)=c('cor','x_mid','y_mid','p','rsq','x_lab','y_lab')
  z_cor$x_lab = ezLev(factor(z_cor$x_lab),names(data))
  z_cor$y_lab = ezLev(factor(z_cor$y_lab),names(data))
  diag = z_cor[z_cor$x_lab==z_cor$y_lab,]
  z_cor=z_cor[z_cor$x_lab!=z_cor$y_lab,]
  #start creating layers
  points_layer = layer(
    geom = 'point'
    , data = z
    , mapping = aes(
      x = x
      , y = y
    )
  )
  lm_line_layer = layer(
    geom = 'line'
    , geom_params = list(colour = 'red')
    , stat = 'smooth'
    , stat_params = list(method = 'lm')
    , data = z
    , mapping = aes(
      x = x
      , y = y
    )
  )
  lm_ribbon_layer = layer(
    geom = 'ribbon'
    , geom_params = list(fill = 'green', alpha = .5)
    , stat = 'smooth'
    , stat_params = list(method = 'lm')
    , data = z
    , mapping = aes(
      x = x
      , y = y
    )
  )
  cor_text = layer(
    geom = 'text'
    , data = z_cor
    , mapping = aes(
      x=y_mid
      , y=x_mid
      , label=cor
      , size = rsq
      , colour = p
    )
  )
  var_text = layer(
    geom = 'text'
    , geom_params = list(size=var_text_size)
    , data = diag
    , mapping = aes(
      x=y_mid
      , y=x_mid
      , label=x_lab
    )
  )
  f = facet_grid(y_lab~x_lab,scales='free')
  o = opts(
    panel.grid.minor = theme_blank()
    ,panel.grid.major = theme_blank()
    ,axis.ticks = theme_blank()
    ,axis.text.y = theme_blank()
    ,axis.text.x = theme_blank()
    ,axis.title.y = theme_blank()
    ,axis.title.x = theme_blank()
    ,legend.position='none'
  )
  size_scale = scale_size(limits = c(0,1),to=cor_text_limits)
  return(
    ggplot()+
      points_layer+
      lm_ribbon_layer+
      lm_line_layer+
      var_text+
      cor_text+
      f+
      o+
      size_scale
  )
}




## Scatterplot matrix highlighting just one variable vs the others ---- 
scatter_matrix_keyvar = function(scat_df, scat_var, point_col, point_size,
                                 legend_pos, ylab, xlab,
                                 axis_size,  axis_title, labelSize,
                                 title_size, leg_size, title) {
  
  plot <- ggplot(scat_df, aes(!!sym(scat_var), value)) +
    geom_point(color = point_col, size = point_size) +
    facet_grid(.~variable) + 
    stat_cor(aes(color = 'red'), method = "spearman", label.y = 1.3) +
    
    ## Use the classic theme
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    
    ## Change the axes sizes, etc.
    theme(axis.title.x     = element_text(colour = "black", size = axis_title, face = "bold"),
          axis.text.x      = element_text(size   = axis_size),
          
          axis.title.y     = element_text(colour = "black", size = axis_title, face = "bold"),
          axis.text.y      = element_text(size   = axis_size),
          
          strip.text.x     = element_text(size   = labelSize, color = "red", face = "bold.italic"),
          strip.text.y     = element_text(size   = labelSize, color = "red", face = "bold.italic"),
          
          panel.background = element_blank(),
          panel.border     = element_rect(colour = "black", fill = NA, size = 1.2),
          plot.title       = element_text(size   = title_size, face = "bold"),
          legend.text      = element_text(size   = leg_size),
          legend.title     = element_blank(),
          legend.position  = legend_pos,
          legend.key.size  = unit(1.5, "cm"))
  
  return(plot)
  
}




## Scatterplot matrix grouped with histogram ---- 
scatter_matrix_grouped_histo = function(scat_df, cols, scat_col,
                                        alpha, alignPer, legend_pos,
                                        upper_size, lower_size, axis_size, labelSize,
                                        title_size, leg_size, title) {
  
  plot <- ggpairs(scat_df,
                  
                  ## Remove the legend and histogram
                  legend  = ncol(scat_df),
                  columns    = cols, 
                  mapping    = ggplot2::aes(colour = !!sym(scat_col)),
                  axisLabels = "show",
                  
                  ## Create upper and lower panels
                  upper   = list(combo      = wrap("box_no_facet", alpha = alpha),
                                 continuous = wrap("cor",          size  = upper_size, 
                                                   alignPercent = alignPer)),
                  diag    = list(continuous = wrap("densityDiag",  alpha = 0.5 )), 
                  lower   = list(continuous = wrap("smooth", 
                                                   alpha = alpha, 
                                                   size  = lower_size))) + 
    
    ## Use the classic theme
    theme_classic() +
    
    ## Change the axes sizes, etc.
    theme(axis.title.x     = element_text(colour = "black", size = axis_size),
          axis.text.x      = element_text(size   = axis_size),
          
          axis.title.y     = element_text(colour = "black", size = axis_size),
          axis.text.y      = element_text(size   = axis_size),
          
          strip.text.x     = element_text(size   = labelSize, color = "red", face = "bold.italic"),
          strip.text.y     = element_text(size   = labelSize, color = "red", face = "bold.italic"),
          
          panel.background = element_blank(),
          panel.border     = element_rect(colour = "black", fill = NA, size = 1.2),
          plot.title       = element_text(size   = title_size, face = "bold"),
          legend.text      = element_text(size   = leg_size),
          legend.title     = element_blank(),
          legend.position  = legend_pos,
          legend.key.size  = unit(1.5, "cm"))
  
  return(plot)
  
}





## Scatterplot matrix grouped ---- 
scatter_matrix_colour_histo = function(scat_df,    cols, scat_col,
                                       alpha, alignPer, legend_pos,
                                       upper_size, lower_size, axis_size, labelSize,
                                       title_size, leg_size, title) {
  
  plot <- ggpairs(scat_df,
                  
                  ## Remove the legend and histogram
                  #legend  = ncol(df),
                  columns = cols,
                  mapping = ggplot2::aes(colour = scat_col),
                  
                  ## Create upper and lower panels
                  upper   = list(combo      = wrap("box_no_facet", alpha = alpha),
                                 continuous = wrap("cor",          size  = upper_size, alignPercent = alignPer)),
                  lower   = list(continuous = wrap("smooth",       alpha = alpha, size = lower_size)),
                  diag    = list(continuous = wrap("densityDiag",  alpha = 0.5 ))) +
    
    ## Use the classic theme
    theme_classic() +
    
    ## Change the axes sizes, etc.
    theme(axis.title.x     = element_text(colour = "black", size = axis_size),
          axis.text.x      = element_text(size   = axis_size),
          
          axis.title.y     = element_text(colour = "black", size = axis_size),
          axis.text.y      = element_text(size   = axis_size),
          
          strip.text.x     = element_text(size   = labelSize, color = "red", face = "bold.italic"),
          strip.text.y     = element_text(size   = labelSize, color = "red", face = "bold.italic"),
          
          panel.background = element_blank(),
          panel.border     = element_rect(colour = "black", fill = NA, size = 1.2),
          plot.title       = element_text(size   = title_size, face = "bold"),
          legend.text      = element_text(size   = leg_size),
          legend.title     = element_blank(),
          legend.position  = legend_pos,
          legend.key.size  = unit(1.5, "cm"))
  
  return(plot)
  
}





## SATISFACTION PLOTS =======================================================================


# highs      = EMU_highs_10
# lows       = EMU_lows_10
# 
# 
# scale_cols     = c('Neutral'     = 'grey',
#                    'Opportunity' = 'skyblue3',
#                    'Risk'        = 'darkorange1')
# 
# leg_order      = c('Risk',
#                    'Neutral',
#                    'Opportunity')
# 
# title      = Label
# # caption    = Count_caption,
# 
# tsize      = 35
# lab_size   = 8
# leg_size   = 25
# ysize      = 25 
# xsize      = 25
# # capt_size  = 20,
# width      = 0.5 
# ymin       = -1.1
# ymax       = 1.1
# high_just  = 0
# low_just   = 1.5



## stacked_liker_percent ---- 
zero_centred_barchart = function(highs,  lows, fill_colm,
                                 # caption, 
                                 # capt_size,
                                 # high_label,   low_label, 
                                 scale_cols,
                                 title, count, lab_size, leg_size, leg_order,
                                 tsize, ysize, xsize, width, 
                                 ymin, ymax, high_just, low_just) {
  
  ## Start the gg device
  ggplot() + 
    
    ## Create the bar charts
    ## use the 'Theme' column for the order
    ## Legend happens with fill = Class reorder(Response, -Risk)
    geom_bar(data = highs, aes(x  = reorder(!!sym(fill_colm), desc(!!sym(fill_colm))), y = Risk, fill = Class), 
             position = "stack", stat = "identity", width = width) +
    
    geom_bar(data = lows,  aes(x  = reorder(!!sym(fill_colm), desc(!!sym(fill_colm))), y = Risk, fill = Class),
             position = "stack", stat = "identity", width = width) +
    
    ## Create bars between the response categories
    geom_hline(yintercept  =  0, color  = "black", size  =  1) +
    theme_light() +
    
    ## Make them the same everytime
    visras_scale_fill_discrete(breaks = leg_order, values = scale_cols) +
    
    ## Add values for responses
    # geom_text(data = high_label, aes(x = !!sym(fill_colm), y = Risk, label = Risk), size = lab_size, hjust =  high_just) +
    # geom_text(data = low_label,  aes(x = !!sym(fill_colm), y = Risk, label = Risk), size = lab_size, hjust =  low_just) +
    
    ## Plot the bars horizontally
    coord_flip() +
    labs(title   = title, 
         # caption = caption,
         y = '', x = '') +
    scale_y_continuous(#labels = scales::percent, 
                       limits = c(ymin,ymax)) +
    
    ## Scale between +/- 50%
    #scale_y_continuous(breaks  = seq( 1, 0.25), limits = c( 1)) +
    
    ## Create titles and legend
    theme(plot.title      =  element_text(size  = tsize, hjust = 0.5,   face = "bold"),
          axis.text.y     =  element_text(hjust = 0,     size  = ysize, face = "bold"),
          axis.text.x     =  element_text(size  = xsize),
          legend.title    =  element_text(size  = leg_size,  face = "bold"),
          legend.text     =  element_text(size  = leg_size),
          legend.position =  "bottom"#,
          # plot.caption    = element_text(size   = capt_size, hjust = 0.5, face = "italic", color = "black")
          ) 
  
  
}





## stacked_liker_percent_single ---- 
zero_centred_barchart_single = function(highs,  lows, caption, capt_size,
                                        high_label, low_label, scale_cols,
                                        title, count, lab_size, leg_size, leg_order,
                                        tsize, ysize, xsize, width, order,
                                        ymin, ymax, high_just, low_just) {
  
  ## Start the gg device
  ggplot() + 
    
    ## Create the bar charts
    ## Legend happens with fill = Class
    geom_bar(data = highs, aes(x  = Role, y = Risk, fill = Class), 
             position = "stack", stat = "identity", width = width) +
    
    geom_bar(data = lows,  aes(x  = Role, y = Risk, fill = Class),
             position = "stack", stat = "identity", width = width) +
    
    ## Create bars between the response categories
    geom_hline(yintercept  =  0, color  = "black", size  =  1) +
    theme_light() +
    
    ## Make them the same everytime
    visras_scale_fill_discrete(breaks = leg_order, values = scale_cols) +
    
    ## Add values for responses
    geom_text(data = high_label, aes(x = Role, y = Risk, label = Risk), size = lab_size, hjust =  high_just) +
    geom_text(data = low_label,  aes(x = Role, y = Risk, label = Risk), size = lab_size, hjust =  low_just) +
    
    ## Plot the bars horizontally
    coord_flip() +
    labs(title   = title, 
         caption = caption,
         y = '', x = '') +
    
    scale_y_continuous(labels=scales::percent, limits=c(ymin, ymax))+
    
    ## Create titles and legend
    theme(plot.title      =  element_text(size  = tsize, hjust = 0.5,   face = "bold"),
          axis.text.y     =  element_blank(),
          axis.text.x     =  element_text(size  = xsize),
          legend.title    =  element_text(size = leg_size,  face = "bold"),
          legend.text     =  element_text(size = leg_size),
          legend.position =  "bottom",
          plot.caption    = element_text(size   = capt_size, hjust = 0.5, face = "italic", color = "black"))
  
}



## HI-CHARTS =======================================================================


## Simple Risk bar function ---- 
simple_Risk_bar = function(df, xcol, ycol, 
                                 ytitle, title, 
                                 categor, subtitle) {
  
  ## Make the df formattable  
  formattable(df)
  highchart() %>% 
    
    ## Data for plotting
    hc_add_series(Q4_table, "bar", hcaes(x = xcol, y = ycol), name = 'Responses') %>%
    
    ## Options for each type of series
    hc_plotOptions(
      series = list(
        showInLegend = FALSE,
        pointFormat = "{point.y}%"
      ),
      column = list(
        colorByPoint = TRUE
      )
    ) %>%
    
    ## Axis
    hc_yAxis(
      title  = list(text   = ytitle),
      labels = list(format = "{value}")
    ) %>% 
    
    hc_xAxis(categories = categor) %>%
    
    ## Titles and credits
    hc_title(
      text = title
    ) %>%
    
    hc_subtitle(text = "This is a single response question") %>%
    
    hc_credits(
      enabled = TRUE, text = paste0(sum(df$Count), " Respondents"),
      href = "#",
      style = list(fontSize = "12px")
    )
}





## Stacked bar percent function ---- 
## https://ox-it.github.io/OxfordIDN_htmlwidgets/charts/StackedBarCharts/
stacked_bar_percent = function(df, categories_column, 
                               measure_columns, title, resp) {
  
  generated_chart <- highchart() %>%
    
    hc_chart(type = "column") %>% 
    hc_title(text = title) %>% 
    hc_subtitle(text = paste0('n = ', resp)) %>% 
    
    hc_xAxis(categories = df[, categories_column],
             title = categories_column)
  
  invisible(lapply(measure_columns, function(column) {
    generated_chart <<-
      hc_add_series(hc = generated_chart, name = column,
                    data = df[, column])
  }))
  
  generated_chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = "percent")) %>%
    hc_yAxis(title = list(text = "Risk %")) %>%
    hc_legend(reversed = TRUE) 
  
}





## INDEX PLOTS =======================================================================


## table(Survey_data$C11)

## Grouped Boxplots ---- 
factor_boxplots = function(df, x_var, y_var,     pallette, group_var, mar,
                           box_size,  x_lab,     y_lab, y_lim, leg_pos,
                           lab_size,  lab_angle, border_size, v_just) {
  
  plot <- ggboxplot(df, 
                    x       = x_var, 
                    y       = y_var, 
                    fill    = group_var,
                    palette = pallette, size = box_size,
                    outlier.shape = NA) +
    
    geom_hline(yintercept = 0, col = 'black', linetype = "dashed") +
    
    ## Use the classic theme
    theme_classic() +
    labs(y = y_lab,
         x = '') +
    scale_y_continuous(limits = y_lim) +
    
    ## Change the axes sizes, etc.
    theme(plot.margin      = unit(c(mar, mar, mar, mar), "cm"),
          axis.title.x     = element_text(colour = 'black',  size  = lab_size),
          axis.text.x      = element_text(size   = lab_size, angle = lab_angle, 
                                          face   = "bold",   hjust = 1),
          
          axis.title.y     = element_text(colour = 'black',  size = lab_size, face = "bold", vjust = v_just),
          axis.text.y      = element_text(size   = lab_size),
          
          panel.border     = element_rect(colour = 'black',  fill = NA, size = border_size),
          plot.title       = element_text(size   = lab_size, face = 'bold'),
          legend.text      = element_text(size   = lab_size, face = 'bold'),
          legend.title     = element_blank(),
          legend.position  = leg_pos,
          legend.key.size  = unit(1.5, 'cm')) +
    
    ## And title
    ggtitle(paste0(''))
  
  return(plot)
  
}





###########################################################################################
############################################ ---TBC---- ####################################
############################################################################################