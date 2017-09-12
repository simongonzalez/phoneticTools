#' filter_parameters %% ~~function to do ... ~~
#' 
#' Filters acoustic values based on user input parameters
#' 
#' Output:
#' 
#' Exports the same dataset with the same number of columns containing the
#' filtered elements
#' 
#' @aliases filter_parameters
#' @param data_set dataset (data.frame) containing the columns of acoustic
#' values to be filtered
#' @param parameter_type character variable containing the acoustic parameter
#' to be filtered
#' @param parameter_header character variable containing the header of the
#' column to have content filtered
#' @param filter_parameter numeric vector containing the values for filtering,
#' i.e. thresholds or specific values conventions:
#' 
#' ... 1. One positive value = ONLY observations below or equal than the value are
#' kept
#' 
#' ... 2. One negative value = ONLY observations above that value are kept
#' 
#' ... 3. Two positive values = ONLY observations between these values are kept
#' @param parameter_subset list containing data subsets for the filtering. If
#' parameter_subset is NULL, all values will be filtered based on the
#' filter_parameter argument.
#' 
#' If the user chooses to have different thresholds for different subsets, a
#' list can be entered specifying thresholds as in the filter_parameter
#' argument, e.g. list('i:' = 500, 'e' = -50, 'u' = c(400, 600))
#' @param std_threshold Filter by Standard Deviations.
#' 
#' If one positive number is entered, all Standard Deviations within that
#' threshold will be kept.
#' 
#' If one negative number is entered, all Standard Deviations out of that
#' threshold will be excluded.
#' @param std_every If std_threshold is selected (not NULL) and std_every is
#' left NULL, Standard Deviations are calculated based on all the observations
#' of the data in the row.
#' 
#' If there is more than one observation point for each token (i.e. more than
#' one point in trajectories), then tokens will have the same Standard
#' Deviations repeated. This will affect the Standard Deviation value.
#' 
#' Thus if std_every is not NULL, then the Standard Deviation values will based
#' on the every number of rows entered, e.g. every five points.
#' @param data_subset If std_threshold is selected (not NULL), Standard
#' Deviations are calculated based on the data_subset input.
#' 
#' One or more subset columns can be selected. If left NULL, the Standard
#' Deviation is calculated based on all values without any groupings.
#' @param delete_uneven boolean for determining whether uneven entries after
#' filtering will be deleted, e.g. if each token MUST have five (5) entries but
#' the filtering left some tokens with three (3) or four (4) entries. For the
#' latter tokens, all entries with eneven data are deleted
#' @param delete_uneven_rows If delete_uneven == TRUE, character vector
#' containing the column headers to be inspected for uneven number of entries
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author Simon Gonzalez - simon.gonzalez@@anu.edu.au
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' @export
#' 
#' new_filtering =  filter_parameters(data_set = input_data,
#'                                    parameter_type = 'pitch',
#'                                    parameter_header = 'pitch',
#'                                    filter_parameter = c(200),
#'                                    delete_uneven = T,
#'                                    delete_uneven_rows = c('overall_token', 'extraction_param'))
#' 
#' 
filter_parameters <- function(data_set = NULL, parameter_type = NULL, parameter_header = NULL,
                              filter_parameter = NULL, parameter_subset = NULL, subset_header = NULL, 
                              std_threshold = NULL, std_every = NULL, data_subset = NULL, delete_uneven_rows = NULL){
  
  #argument check====================================================================================================
  assertDataFrame(data_set)
  
  expect_character(parameter_type, len = 1)
  if(!testSubset(parameter_type, c('duration', 'frequency', 'intensity', 'pitch')))
    stop("No parameter with the label specified in parameter_type. Accepted labels: duration, frequency, intensity, pitch.")
  
  expect_character(parameter_header, len = 1)
  if(!testSubset(parameter_header, colnames(data_set)))
    stop("No columns in data_set with the specified label in parameter_header.")
  
  if(!is.null(filter_parameter)){
    expect_numeric(filter_parameter, min.len = 1, max.len = 2)
    if(length(filter_parameter) == 1 & filter_parameter[1] == 0)
      stop("The value entered in filter_parameter cannot be == 0.")
    
    if(length(filter_parameter) == 2 & length(which(filter_parameter <= 0)) > 0)
      stop("If twovalues are entered, both MUST be positive values.")
  }
  
  
  if(!is.null(parameter_subset))
    expect_list(parameter_subset, min.len = 1, info =  "e.g. list('i:' = 500, 'e' = -50, 'u' = c(400, 600))")
  
  if(!is.null(parameter_subset)){
    expect_character(subset_header, len = 1)
    if(!testSubset(subset_header, colnames(data_set)))
      stop("No columns in data_set with the specified label in subset_header.")
  }
  
  if(!is.null(std_threshold)){
    expect_numeric(std_threshold, len = 1)
    if(std_threshold == 0)
      stop("The value entered in std_threshold MUST be either positive (>0) or negative (<0).")
  }
  
  if(!is.null(std_threshold))
    if(!is.null(std_every))
      expect_numeric(std_every, len = 1, lower = 2)
  
  if(!is.null(data_subset)){
    expect_character(data_subset, min.len = 1, max.len = length(colnames(data_set)))
    if(!testSubset(data_subset, colnames(data_set)))
      stop("No columns in data_set with the specified label(s) in data_subset.")
  }
  
  
  if(!is.null(delete_uneven_rows)){
    expect_character(delete_uneven_rows, len = 2)
    if(!testSubset(delete_uneven_rows, colnames(data_set)))
      stop("No columns in data_set with the specified label(s) in delete_uneven_rows.")
  }
  
  #parameter Standard deviation filtering==========================================================================================
  #================================================================================================================================
  #================================================================================================================================
  if(!is.null(std_threshold)){
    
    if(!is.null(std_every)){
      
      if(is.character(std_every)){
        std_every <- max(data_set[[std_every]])
      }
      
      tmp_data_set <- data_set[seq(1, nrow(data_set), std_every),]
    }else{
      tmp_data_set <- data_set
    }
    
    if(!is.null(data_subset)){
      main_values <- ddply( tmp_data_set, .variables = data_subset,
                            summarize, mean_value=mean(get(parameter_header)), sd_value=sd(get(parameter_header), na.rm = F))
      
      main_values[is.na(main_values)] <- 0
      
      tmp_logical_text <- c()
      
      for(tmp_text in 1:length(data_subset)){
        if(tmp_text < length(data_subset)){
          tmp_logical_text <- paste0(tmp_logical_text, "data_set[[\"", data_subset[tmp_text], "\"]] == \"", main_values[[data_subset[tmp_text]]], "\" & ")
        }else{
          tmp_logical_text <- paste0(tmp_logical_text, "data_set[[\"", data_subset[tmp_text], "\"]] == \"", main_values[[data_subset[tmp_text]]], "\"")
        }
      }
      for(subset_values in 1:nrow(main_values)){
        mn_value <- main_values[subset_values,c('mean_value')]
        sd_value <- main_values[subset_values,c('sd_value')]
        
        lower_threshold <- mn_value - (sd_value * std_threshold)
        upper_threshold <- mn_value + (sd_value * std_threshold)
        
        logical_filter <- eval(parse(text = tmp_logical_text[subset_values]))
        
        if(std_threshold > 0){
          data_set <- data_set[!(logical_filter &
                                   (data_set[[parameter_header]] < lower_threshold |
                                      data_set[[parameter_header]] > upper_threshold)), ]
        }else if(std_threshold < 0){
          data_set <- data_set[!(logical_filter &
                                   (data_set[[parameter_header]] > lower_threshold &
                                      data_set[[parameter_header]] < upper_threshold)), ]
        }
      }
    }else{
      mn_value <- mean(data_set[[parameter_header]])
      sd_value <- sd(data_set[[parameter_header]])
      
      lower_threshold <- mn_value - (sd_value * std_threshold)
      upper_threshold <- mn_value + (sd_value * std_threshold)
      
      if(std_threshold > 0){
        data_set <- data_set[data_set[[parameter_header]] >= lower_threshold & data_set[[parameter_header]] <= upper_threshold,]
      }else if(std_threshold < 0){
        data_set <- data_set[data_set[[parameter_header]] < lower_threshold | data_set[[parameter_header]] > upper_threshold,]
      }
    }
  }
  
  #parameter filtering=============================================================================================================
  #================================================================================================================================
  #================================================================================================================================
  
  if(!is.null(filter_parameter)){
    
    if(length(filter_parameter) == 1){
      
      if(filter_parameter > 0){

        data_set <- data_set[data_set[[parameter_header]] <= filter_parameter,]
        
      }else if(filter_parameter < 0){
        filter_parameter <- filter_parameter * -1
        
        data_set <- data_set[data_set[[parameter_header]] > filter_parameter,]
      }
    }else{
      
      lower_boundary <- filter_parameter[1]
      upper_boundary <- filter_parameter[2]
      
      data_set <- data_set[data_set[[parameter_header]] >= lower_boundary & data_set[[parameter_header]] <= upper_boundary,]
    }
  }
  
  #------------------------------------------------------------------------------------------------------------------------------
  
  if(!is.null(parameter_subset)){
    
    for(parameter_subset_i in 1:length(parameter_subset)){

      if(length(parameter_subset[[parameter_subset_i]]) == 1){

        if(parameter_subset[[parameter_subset_i]] > 0){
          
          tmp_segment_threshold <- parameter_subset[[parameter_subset_i]]
          
          if(!is.null(subset_header)){

            data_set <- data_set[!(data_set[[subset_header]] == names(parameter_subset)[parameter_subset_i] &
                                     data_set[[parameter_header]] > tmp_segment_threshold),]
            View(data_set)
          }
          
        }else if(parameter_subset[[parameter_subset_i]] < 0){
          
          tmp_segment_threshold <- parameter_subset[[parameter_subset_i]] * -1
          
          if(!is.null(subset_header)){
            data_set <- data_set[!(data_set[[subset_header]] == names(parameter_subset)[parameter_subset_i] &
                                     data_set[[parameter_header]] <= tmp_segment_threshold),]
          }
        }
      }else{
        
        lower_boundary <- parameter_subset[[parameter_subset_i]][1]
        upper_boundary <- parameter_subset[[parameter_subset_i]][2]
        
        if(!is.null(subset_header)){
          data_set <- data_set[!(data_set[[subset_header]] == names(parameter_subset)[parameter_subset_i] &
                                   (data_set[[parameter_header]] < lower_boundary | data_set[[parameter_header]] > upper_boundary)),]
        }
      }
    }
  }
  
  if(!is.null(delete_uneven_rows)){
    #gets the count for each token
    tmp_count <- plyr::count(data_set, c(delete_uneven_rows[1]))
    
    #gets uneven tokens
    tmp_token_number <- tmp_count[tmp_count$freq != max(data_set[[delete_uneven_rows[2]]]),c(delete_uneven_rows[1])]
    
    #deletes uneven tokens
    data_set <- data_set[!(data_set[[delete_uneven_rows[1]]] %in% tmp_token_number),]
  }
  
  return(data_set)
}
