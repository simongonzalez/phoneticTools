library(ggplot2)
library(ggthemes)
library(plotly)

plot_distribution <- function(data_set = NULL, plot_type = 'bars', column_header = NULL, subset_column_header = NULL, subset_label = NULL, point_column = NULL, select_point = NULL, distribution_plot_single = 'merge', multicolor = FALSE, central_tendency = 'mean', stack_distributions = TRUE, x_trim = NULL, y_trim = NULL, plot_theme = 'theme_minimal', plot_interactive = TRUE, save_plot = FALSE, save_name = NULL){
  
  # data_set = NULL input dataset
  # plot_type = 'Bars' plot type, either bar plots and/or densities
  # column_header = NULL column header of the values to be plotted
  # subset_column_header = NULL if the plots are to be subsetted, provided the subset column header
  # subset_label = NULL the labels in the subset column to be plotted
  # point_column = NULL if there is a poiint column, if the values in the column are divided in separate points, like in long datasets
  # select_point = NULL if there are several points, select the points to be plotted, if left blank, all points are plotted
  # distribution_plot_single = NULL if there are several categories/groups, the options on how to plot them: merge, split, grid
  # multicolor = FALSE, in case of categorical plots, if each category is plotted with only one color or multiple colors
  # central_tendency = NULL plotting the mean line and/or the median line: c('mean', 'median'), c('mean'), c('median')
  # stack_distributions = T if multiple variables are plotted in the same plot, gives the option of staking the variables
  # x_trim = NULL, manual x axis limits
  # y_trim = NULL, manual y axis limits
  # plot_theme = 'theme_minimal', selection of ggplot theme
  # plot_interactive = TRUE, if the plot should be interactive using plotly, otherwise, it is a static plot using only ggplot
  # save_plot = FALSE, if the plot should be saved as a .png file
  # save_name = NULL, if left NULL, the plot is saved with the system date (Sys.Date()), otherwise, it is saved with the name provided
  
  #argument check====================================================================================================
  assertDataFrame(data_set)
  
  expect_character(plot_type, min.len = 1, max.len = 2)
  if(!testSubset(plot_type, c('bars', 'densities')))
    stop("No plot with the label specified in plot_type. Accepted labels: bars, densities.")
  
  expect_character(column_header, len = 1)
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label in column_header.")
  
  if (!is.null(subset_column_header)){
    expect_character(subset_column_header, len = 1)
    if(!testSubset(subset_column_header, colnames(data_set)))
      stop("No columns in data_set with the specified label in subset_column_header.")
  }

  if(!is.null(subset_column_header)){
    if (!is.null(subset_label)){
    #get unique labels from the new column
    check_unique_labels <- as.character(unique(data_set[[subset_column_header]]))
    print(check_unique_labels)
    
    expect_character(subset_label, min.len = 1, max.len = length(check_unique_labels))
    if(!testSubset(subset_label, check_unique_labels))
      stop("At least one label in subset_label is not found in subset_label.")
    }
    
    if(length(subset_label) > 1){
      
      if(is.numeric(data_set[[column_header]])){
        expect_character(distribution_plot_single, len = 1)
        if(!testSubset(distribution_plot_single, c('merge', 'split', 'grid')))
          stop("No plot type with the label specified in distribution_plot_single. Accepted labels: merge, split, grid.")
      }else{
        expect_character(distribution_plot_single, len = 1)
        if(!testSubset(distribution_plot_single, c('merge', 'split', 'equal', 'grid')))
          stop("No plot type with the label specified in cdistribution_plot_single. Accepted labels: merge, split, equal, grid.")
      }

    }
  }
  
  if(!is.null(point_column)){
    expect_character(point_column, len = 1)
    if(!testSubset(point_column, colnames(data_set)))
      stop("No columns in data_set with the specified label in point_column.")
  }
  
  if(!is.null(point_column)){
    if (!is.null(select_point)){
      #get unique labels from the new column
      check_unique_labels <- unique(data_set[[point_column]])
      
      expect_numeric(select_point, lower = min(check_unique_labels), upper = max(check_unique_labels), min.len = 1, unique = length(check_unique_labels))
    }
  }
  
  if (multicolor != FALSE & multicolor != TRUE)
    stop("multicolor MUST be BOOLEAN - TRUE or FALSE")
  
  expect_character(central_tendency, min.len = 1, max.len = 2)
  if(!testSubset(central_tendency, c('mean', 'median')))
    stop("No central tendency with the label specified in central_tendency. Accepted labels: mean, median.")
  
  if (stack_distributions != FALSE & stack_distributions != TRUE)
    stop("stack_distributions MUST be BOOLEAN - TRUE or FALSE")
  
  if (!is.null(x_trim))
    expect_numeric(x_trim, len = 2)
  
  if (!is.null(y_trim))
    expect_numeric(y_trim, len = 2)
  
  
  all_inside_themes <- unlist(strsplit('theme_grey theme_gray theme_bw theme_linedraw theme_light theme_dark theme_minimal theme_classic theme_void theme_test theme_base theme_calc theme_economist theme_excel theme_few theme_fivethirtyeight theme_gdocs theme_hc theme_par theme_pander theme_solarized theme_stata theme_tufte theme_wsj', ' '))
  
  expect_character(plot_theme, len = 1)
  if(!testSubset(plot_theme, all_inside_themes))
    stop(paste0("No theme with the label specified in plot_theme. Accepted labels: ", all_inside_themes, "."))
  
  if (plot_interactive != FALSE & plot_interactive != TRUE)
    stop("plot_interactive MUST be BOOLEAN - TRUE or FALSE")
  
  if (save_plot != FALSE & save_plot != TRUE)
    stop("save_plot MUST be BOOLEAN - TRUE or FALSE")
  
  
  #==============================================================================================================================
  #==============================================================================================================================
  #==============================================================================================================================
  
  #if all the data points are to be plotted, the point_column is empty
  #otherwise, a specific point is speficied
  if(!is.null(point_column)){
    if(point_column != '' & point_column != ' '){
      #subsets the data on the specified column to the point selected
      data_set <- data_set[data_set[[column_header]] == point_column,]
    }
  }
  
  #there are two types of distribution plots, one for numeric and another for categorical
  #this is for the numeric data
  if(is.numeric(data_set[[column_header]])){
    
    #if a subset_column_header has been entered and an specified label inside on that column has been entered
    if(!is.null(subset_column_header)){
      if(subset_column_header != "" & !is.null(subset_label)){
        data_set <- data_set[data_set[[subset_column_header]] %in% subset_label,]
      }
    }
    
    #sets a string with the subset_label. This is used for the plotting
    x = column_header
    
    #if the subset is more than one category
    if(length(subset_label) > 1){
      
      #creates the basic plot, with the subset label(s)
      ggp <- ggplot(data_set,aes_string(x=x))
      
      #gives the user three options of dealing with multiple categories:
      #merge = combines all values in one plotn without any distinction
      #split = combines both plots in a single plot but colour coded separated
      #grid = plots categories in different plots arranged in a grid
      
      #merge option
      if(distribution_plot_single == 'merge'){
        
        #if user requests only the Bars plotting
        if(length(plot_type) == 1 & length(which('bars' %in% plot_type)) == 1){
          ggp = ggp + geom_histogram(fill="#0072B2", colour =  "#0072B2", alpha = 0.7)
          
          ggp = ggp + geom_vline(aes_string(xintercept=mean(x, na.rm=T)),   # Ignore NA values for mean
                                 color="red", linetype="dashed", size=1)
          
          ggp = ggp + theme(legend.title = element_blank())
          
        }else if(length(plot_type) == 1 & length(which('densities' %in% plot_type)) == 1){
          #if user requests only the Density plotting 
          ggp <- ggp + geom_density(fill = "#ff4d4d", alpha = 0.5)
          ggp = ggp + theme(legend.title = element_blank())
          
        }else if(length(which(c('bars', 'densities') %in% plot_type)) != 0){
          #if user requests both Bars and Densities
          ggp <- ggp + geom_histogram(aes(y = ..density..), alpha = 0.7, fill="#0072B2", colour =  "#0072B2") + 
            geom_density(fill = "#ff4d4d", alpha = 0.5)
          ggp = ggp + theme(legend.title = element_blank())
        }
        
        #if user requests mean lines to be plotted
        if(length(which('mean' %in% central_tendency)) == 1){
          x_mean = 'mean(data_set[[x]], na.rm = TRUE)'
          ggp = ggp + geom_vline(aes_string(xintercept=x_mean), color="red", linetype="solid", size=0.7)
        }
        
        #if user requests median lines to be plotted
        if(length(which('median' %in% central_tendency)) == 1){
          x_median = 'median(data_set[[x]], na.rm = TRUE)'
          ggp = ggp + geom_vline(aes_string(xintercept=x_median), color="blue", linetype="dotted", size=0.7)
        }
        
      }else if(distribution_plot_single == 'split'){
        
        #if user requests only the Bars plotting
        if(length(plot_type) == 1 & length(which('bars' %in% plot_type)) == 1){
          fill_str = subset_column_header
          
          #if user chooses to stack the distributions
          if(stack_distributions == T){
            ggp = ggp +  geom_histogram(aes_string(fill = fill_str, alpha = 0.7), position = 'stack')
          }else{
            ggp = ggp +  geom_histogram(aes_string(fill = fill_str, alpha = 0.7), position = 'identity')
            
            
            #if user requests mean lines to be plotted
            if(length(which('mean' %in% central_tendency)) == 1){
              summary_data_mean = as.data.frame(data_set %>% group_by_(.dots = fill_str) %>%
                                                  summarise_(mean_val = interp(~mean(var, na.rm = TRUE), var = as.name(x))))
              value_string = 'mean_val'
              
              ggp = ggp +  geom_vline(data = summary_data_mean, aes_string(xintercept=value_string,  colour=fill_str),
                                      linetype="solid", size=0.7)
            }
            
            #if user requests median lines to be plotted
            if(length(which('median' %in% central_tendency)) == 1){
              summary_data_median = as.data.frame(data_set %>% group_by_(.dots = fill_str) %>%
                                                    summarise_(mean_val = interp(~median(var, na.rm = TRUE), var = as.name(x))))
              value_string = 'mean_val'
              
              ggp = ggp +  geom_vline(data = summary_data_median, aes_string(xintercept=value_string,  colour=fill_str),
                                      linetype="dotted", size=0.7)
            }
            
          }
          
        }else if(length(plot_type) == 1 & length(which('densities' %in% plot_type)) == 1){
          #if user requests only the Density plotting
          
          fill_str = subset_column_header
          
          #if user chooses to stack the distributions
          if(stack_distributions == T){
            ggp = ggp +  geom_density(aes_string(fill = fill_str, alpha = 0.5), position = "stack")
          }else{
            ggp = ggp +  geom_density(aes_string(fill = fill_str, alpha = 0.5), position = 'identity')
            
            
            #if user requests mean lines to be plotted
            if(length(which('mean' %in% central_tendency)) == 1){
              summary_data_mean = as.data.frame(data_set %>% group_by_(.dots = fill_str) %>%
                                                  summarise_(mean_val = interp(~mean(var, na.rm = TRUE), var = as.name(x))))
              value_string = 'mean_val'
              
              ggp = ggp +  geom_vline(data = summary_data_mean, aes_string(xintercept=value_string,  colour=fill_str),
                                      linetype="solid", size=0.7)
            }
            
            #if user requests median lines to be plotted
            if(length(which('median' %in% central_tendency)) == 1){
              summary_data_median = as.data.frame(data_set %>% group_by_(.dots = fill_str) %>%
                                                    summarise_(mean_val = interp(~median(var, na.rm = TRUE), var = as.name(x))))
              value_string = 'mean_val'
              
              ggp = ggp +  geom_vline(data = summary_data_median, aes_string(xintercept=value_string,  colour=fill_str),
                                      linetype="dotted", size=0.7)
            }
            
            
          }
          
        #if user requests both Bars and Densities
        }else if(length(which(c('bars', 'densities') %in% plot_type)) != 0){
          fill_str = subset_column_header
          y_density = '..density..'
          
          #if user chooses to stack the distributions
          if(stack_distributions == T){
            ggp <- ggp + geom_histogram(aes_string(y = y_density, fill = fill_str), alpha = 0.7, position = 'stack') + 
              geom_density(aes_string(fill = fill_str, alpha = 0.5), position = 'stack')
          }else{
            ggp <- ggp + geom_histogram(aes_string(y = y_density, fill = fill_str), alpha = 0.7, position = 'identity') + 
              geom_density(aes_string(fill = fill_str, alpha = 0.5), position = 'identity')
            
            #if user requests mean lines to be plotted
            if(length(which('mean' %in% central_tendency)) == 1){
              summary_data_mean = as.data.frame(data_set %>% group_by_(.dots = fill_str) %>%
                                                  summarise_(mean_val = interp(~mean(var, na.rm = TRUE), var = as.name(x))))
              value_string = 'mean_val'
              
              ggp = ggp +  geom_vline(data = summary_data_mean, aes_string(xintercept=value_string,  colour=fill_str),
                                      linetype="solid", size=0.7)
            }
            
            #if user requests median lines to be plotted
            if(length(which('median' %in% central_tendency)) == 1){
              summary_data_median = as.data.frame(data_set %>% group_by_(.dots = fill_str) %>%
                                                    summarise_(mean_val = interp(~median(var, na.rm = TRUE), var = as.name(x))))
              value_string = 'mean_val'
              
              ggp = ggp +  geom_vline(data = summary_data_median, aes_string(xintercept=value_string,  colour=fill_str),
                                      linetype="dotted", size=0.7)
            }
          }
          
        }
        
      }else if(distribution_plot_single == 'grid'){
        
        if(length(plot_type) == 1 & length(which('bars' %in% plot_type)) == 1){
          facet_string = subset_column_header
          ggp = ggp + geom_histogram(fill="#0072B2", colour =  "#0072B2")
          
          #if user requests mean lines to be plotted
          if(length(which('mean' %in% central_tendency)) == 1){
            summary_data_mean = as.data.frame(data_set %>% group_by_(.dots = facet_string) %>%
                                                summarise_(mean_val = interp(~mean(var, na.rm = TRUE), var = as.name(x))))
            value_string = 'mean_val'
            
            ggp = ggp +  geom_vline(data = summary_data_mean, aes_string(xintercept=value_string,  colour=facet_string),
                                    linetype="solid", size=0.7)
          }
          
          #if user requests median lines to be plotted
          if(length(which('median' %in% central_tendency)) == 1){
            summary_data_median = as.data.frame(data_set %>% group_by_(.dots = facet_string) %>%
                                                  summarise_(mean_val = interp(~median(var, na.rm = TRUE), var = as.name(x))))
            value_string = 'mean_val'
            
            ggp = ggp +  geom_vline(data = summary_data_median, aes_string(xintercept=value_string,  colour=facet_string),
                                    linetype="dotted", size=0.7)
          }
          
          ggp = ggp + facet_wrap(facet_string)
          ggp = ggp + theme(legend.title = element_blank())
          
        }else if(length(plot_type) == 1 & length(which('densities' %in% plot_type)) == 1){
          facet_string = subset_column_header
          
          ggp = ggp + geom_density(fill="#ff4d4d", alpha = 0.5)
          
          #if user requests mean lines to be plotted
          if(length(which('mean' %in% central_tendency)) == 1){
            summary_data_mean = as.data.frame(data_set %>% group_by_(.dots = facet_string) %>%
                                                summarise_(mean_val = interp(~mean(var, na.rm = TRUE), var = as.name(x))))
            value_string = 'mean_val'
            
            ggp = ggp +  geom_vline(data = summary_data_mean, aes_string(xintercept=value_string,  colour=facet_string),
                                    linetype="solid", size=0.7)
          }
          
          #if user requests median lines to be plotted
          if(length(which('median' %in% central_tendency)) == 1){
            summary_data_median = as.data.frame(data_set %>% group_by_(.dots = facet_string) %>%
                                                  summarise_(mean_val = interp(~median(var, na.rm = TRUE), var = as.name(x))))
            value_string = 'mean_val'
            
            ggp = ggp +  geom_vline(data = summary_data_median, aes_string(xintercept=value_string,  colour=facet_string),
                                    linetype="dotted", size=0.7)
          }
          
          ggp = ggp + facet_wrap(facet_string)
          ggp = ggp + theme(legend.title = element_blank())
          
        }else if(length(which(c('bars', 'densities') %in% plot_type)) != 0){
          
          facet_string = subset_column_header
          
          ggp = ggp + geom_histogram(aes(y = ..density..), alpha = 0.7, fill="#0072B2", colour =  "#0072B2") + 
            geom_density(fill = "#ff4d4d", alpha = 0.5)
          
          #if user requests mean lines to be plotted
          if(length(which('mean' %in% central_tendency)) == 1){
            summary_data_mean = as.data.frame(data_set %>% group_by_(.dots = facet_string) %>%
                                                summarise_(mean_val = interp(~mean(var, na.rm = TRUE), var = as.name(x))))
            value_string = 'mean_val'
            
            ggp = ggp +  geom_vline(data = summary_data_mean, aes_string(xintercept=value_string,  colour=facet_string),
                                    linetype="solid", size=0.7)
          }
          
          #if user requests median lines to be plotted
          if(length(which('median' %in% central_tendency)) == 1){
            summary_data_median = as.data.frame(data_set %>% group_by_(.dots = facet_string) %>%
                                                  summarise_(mean_val = interp(~median(var, na.rm = TRUE), var = as.name(x))))
            value_string = 'mean_val'
            
            ggp = ggp +  geom_vline(data = summary_data_median, aes_string(xintercept=value_string,  colour=facet_string),
                                    linetype="dotted", size=0.7)
          }
          
          ggp = ggp + facet_wrap(facet_string)
          ggp = ggp + theme(legend.title = element_blank())
        }
        
      }
    }else{
      #if the subset is one category
      
      #if user requests only the Bars plotting
      if(length(plot_type) == 1 & length(which('bars' %in% plot_type)) == 1){
        ggp <- ggplot(data_set,aes_string(x=x)) + geom_histogram(fill="#0072B2", colour =  "#0072B2")
      }else if(length(plot_type) == 1 & length(which('densities' %in% plot_type)) == 1){
        #if user requests only the Density plotting 
        ggp <- ggplot(data_set,aes_string(x=x)) + geom_density(fill = "#ff4d4d", alpha = 0.5)
      }else if(length(which(c('bars', 'densities') %in% plot_type)) != 0){
        #if user requests both Bars and Densities
        ggp <- ggplot(data_set,aes_string(x=x)) + geom_histogram(aes(y = ..density..), alpha = 0.7, fill="#0072B2", colour =  "#0072B2") + 
          geom_density(fill = "#ff4d4d", alpha = 0.5)
      }
      
      #if user requests mean lines to be plotted
      if(length(which('mean' %in% central_tendency)) == 1){
        x_mean = 'mean(data_set[[x]], na.rm = TRUE)'
        ggp = ggp + geom_vline(aes_string(xintercept=x_mean), color="red", linetype="solid", size=0.7)
      }
      
      #if user requests median lines to be plotted
      if(length(which('median' %in% central_tendency)) == 1){
        x_median = 'median(data_set[[x]], na.rm = TRUE)'
        ggp = ggp + geom_vline(aes_string(xintercept=x_median), color="blue", linetype="dotted", size=0.7)
      }
      
    }
    
    if (!is.null(x_trim)){
      ggp = ggp + scale_x_continuous(limits = x_trim)
    }
    
    if (!is.null(y_trim)){
      ggp = ggp + scale_y_continuous(limits = y_trim)
    }
    
    ggp = ggp + get(plot_theme)()
    
    ggp = ggp + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    if(save_plot){
      if(!is.null(save_name)){
        ggsave(paste0(save_name, '.png'))
      }else{
        ggsave(paste0(Sys.time(), '.png'))
      }
    }
    
    if(plot_interactive){
      ggplotly(ggp)
    }else{
      ggp
    }
    
  }else{
    
    
    #if a subset_column_header has been entered and an specified label inside on that column has been entered
    if(!is.null(subset_column_header)){
      if(subset_column_header != "" & !is.null(subset_label)){
        data_set <- data_set[data_set[[subset_column_header]] %in% subset_label,]
      }
    }

    #sets a string with the subset_label. This is used for the plotting
    x = column_header
    
    #if the subset is more than one category
    if(length(subset_label) > 1){
      
      #creates the basic plot, with the subset label(s)
      ggp <- ggplot(data_set,aes_string(x=x))
      
      if(distribution_plot_single == 'merge'){
        facet_string = subset_column_header
        
        if(multicolor){
          ggp = ggp + geom_bar(aes_string(fill = x), stat = "count", show.legend = T)
        }else{
          ggp = ggp + geom_bar(stat = "count", fill="#0072B2", colour =  "#0072B2")
        }

        ggp = ggp + theme(legend.title = element_blank())
      }else if(distribution_plot_single == 'split'){
        fill_str = subset_column_header
        ggp = ggp +  geom_bar(stat = "count", aes_string(fill = fill_str))
        
      }else if(distribution_plot_single == 'equal'){
        fill_str = subset_column_header
        ggp = ggp +  geom_bar(stat = "count", aes_string(fill = fill_str), position = 'fill')
      }else if(distribution_plot_single == 'grid'){
        
        facet_string = subset_column_header
        
        if(multicolor){
          ggp = ggp + geom_bar(aes_string(fill = x), stat = "count", show.legend = T)
        }else{
          ggp = ggp + geom_bar(stat = "count", fill="#0072B2", colour =  "#0072B2")
        }
        
        
        
        ggp = ggp + facet_wrap(facet_string)
        ggp = ggp + theme(legend.title = element_blank())
      }
    }else{
      #if the subset is  one category
      
      if(multicolor){
        ggp <- ggplot(data_set,aes_string(x=x)) + geom_bar(aes_string(fill = x), stat = "count")
      }else{
        ggp <- ggplot(data_set,aes_string(x=x)) + geom_bar(stat = "count", fill="#0072B2", colour =  "#0072B2")
      }
      
    }
    
    if (!is.null(x_trim)){
      ggp = ggp + scale_x_continuous(limits = x_trim)
    }
    
    if (!is.null(y_trim)){
      ggp = ggp + scale_y_continuous(limits = y_trim)
    }

    ggp = ggp + get(plot_theme)()
    
    ggp = ggp + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    if(save_plot){
      if(!is.null(save_name)){
        ggsave(paste0(save_name, '.png'))
      }else{
        ggsave(paste0(Sys.time(), '.png'))
      }
    }
    
    if(plot_interactive){
      ggplotly(ggp)
    }else{
      ggp
    }

 
  }
  
}
  


