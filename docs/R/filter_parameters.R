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
#' i.e. thesholds or specific values conventions:
#' 
#' ... 1. One positive values = ONLY observations below or equal that value are
#' kept
#' 
#' ... 2. One negative values = ONLY observations above that value are kept
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
#' of the data, this is, all rows.
#' 
#' If there is more than one observation point for each token (i.e. more than
#' one point in trajectories), then tokens will have the same Standard
#' Deviations repeated. This will affect the Standard Deviation value.
#' 
#' Thus if std_every is not NULL, then the Standard Deviation values will based
#' on the every number of rows entered.
#' @param data_subset If std_threshold is selected (not NULL), Standard
#' Deviations are calculated based on the data_subset input.
#' 
#' One or more subset columns can be selected. If left NULL, the Standard
#' Deviation is calculated based on all values without any groupings.
#' @param delete_uneven boolean for determining whether uneven entries after
#' filtering will be deleted, e.g. if each token MUST have five (5) entries but
#' the filtering left some tokens with three (3) or four (4) entries. For the
#' latter tokens, all entries with eneven data are deleted
#' @param delete_uneven_columns If delete_uneven == TRUE, character vector
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
#'                                    delete_uneven_columns = c('overall_token', 'extraction_param'))
#' 
#' 
filter_parameters <- function(data_set = NULL, parameter_type = NULL, parameter_header = NULL,
                             filter_parameter = NULL, parameter_subset = NULL, parameter_subset_header = NULL, std_threshold = 1, std_every = NULL, data_subset = NULL,
                             delete_uneven = T, delete_uneven_columns = NULL){

  #parameter Standard deviation filtering==========================================================================================
  #================================================================================================================================
  #================================================================================================================================
  if(!is.null(std_threshold)){

    if(!is.null(std_every)){

      if(is.character(std_every)){
        std_every = max(data_set[[std_every]])
      }

      tmp_data_set = data_set[seq(1, nrow(data_set), std_every),]
    }else{
      tmp_data_set = data_set
    }

    if(!is.null(data_subset)){
      main_values = ddply( tmp_data_set, .variables = data_subset,
                           summarize, mean_value=mean(get(parameter_header)), sd_value=sd(get(parameter_header), na.rm = F))

      main_values[is.na(main_values)] = 0

      tmp_logical_text = c()

      for(tmp_text in 1:length(data_subset)){
        if(tmp_text < length(data_subset)){
          tmp_logical_text = paste0(tmp_logical_text, "data_set[[\"", data_subset[tmp_text], "\"]] == \"", main_values[[data_subset[tmp_text]]], "\" & ")
        }else{
          tmp_logical_text = paste0(tmp_logical_text, "data_set[[\"", data_subset[tmp_text], "\"]] == \"", main_values[[data_subset[tmp_text]]], "\"")
        }
      }
      for(subset_values in 1:nrow(main_values)){
        mn_value = main_values[subset_values,c('mean_value')]
        sd_value = main_values[subset_values,c('sd_value')]

        lower_threshold = mn_value - (sd_value * std_threshold)
        upper_threshold = mn_value + (sd_value * std_threshold)

        logical_filter = eval(parse(text = tmp_logical_text[subset_values]))

        if(std_threshold > 0){
          data_set = data_set[!(logical_filter &
                                  (data_set[[parameter_header]] < lower_threshold |
                                     data_set[[parameter_header]] > upper_threshold)), ]
        }else if(std_threshold < 0){
          data_set = data_set[!(logical_filter &
                                  (data_set[[parameter_header]] > lower_threshold &
                                     data_set[[parameter_header]] < upper_threshold)), ]
        }
      }
    }else{
      mn_value = mean(data_set[[parameter_header]])
      sd_value = sd(data_set[[parameter_header]])

      lower_threshold = mn_value - (sd_value * std_threshold)
      upper_threshold = mn_value + (sd_value * std_threshold)

      if(std_threshold > 0){
        data_set = data_set[data_set[[parameter_header]] >= lower_threshold & data_set[[parameter_header]] <= upper_threshold,]
      }else if(std_threshold < 0){
        data_set = data_set[data_set[[parameter_header]] < lower_threshold | data_set[[parameter_header]] > upper_threshold,]
      }
    }
  }

  #parameter filtering=============================================================================================================
  #================================================================================================================================
  #================================================================================================================================

  if(!is.null(filter_parameter)){

    if(length(filter_parameter) == 1){

      if(filter_parameter > 0){

        if(parameter_type == 'duration'){
          filter_parameter = filter_parameter / 1000
        }

        data_set = data_set[data_set[[parameter_header]] <= filter_parameter,]

      }else if(filter_parameter < 0){

        if(parameter_type == 'duration'){
          filter_parameter = filter_parameter / 1000 * -1
        }else{
          filter_parameter = filter_parameter * -1
        }

        data_set = data_set[data_set[[parameter_header]] > filter_parameter,]
      }
    }else{

      if(parameter_type == 'duration'){
        lower_boundary = filter_parameter[1] / 1000
        upper_boundary = filter_parameter[2] / 1000
      }else{
        lower_boundary = filter_parameter[1]
        upper_boundary = filter_parameter[2]
      }

      data_set = data_set[data_set[[parameter_header]] >= lower_boundary & data_set[[parameter_header]] <= upper_boundary,]
    }
  }

  #------------------------------------------------------------------------------------------------------------------------------

  if(!is.null(parameter_subset)){

    for(parameter_subset_i in 1:length(parameter_subset)){

      if(length(parameter_subset[[parameter_subset_i]]) == 1){

        if(parameter_subset[[parameter_subset_i]] > 0){

          if(!is.null(parameter_type) || parameter_type == 'duration'){
            #convert to ms
            tmp_segment_threshold = parameter_subset[[parameter_subset_i]] / 1000
          }else{
            tmp_segment_threshold = parameter_subset[[parameter_subset_i]]
          }

          if(!is.null(parameter_subset_header)){
            data_set = data_set[!(data_set[[parameter_subset_header]] == names(parameter_subset)[parameter_subset_i] &
                                  data_set[[parameter_header]] > tmp_segment_threshold),]
          }

        }else if(parameter_subset[[parameter_subset_i]] < 0){

          if(!is.null(parameter_type) || parameter_type == 'duration'){
            #convert to ms
            tmp_segment_threshold = parameter_subset[[parameter_subset_i]] / 1000 * -1
          }else{
            tmp_segment_threshold = parameter_subset[[parameter_subset_i]] * -1
          }

          if(!is.null(parameter_subset_header)){
            data_set = data_set[!(data_set[[parameter_subset_header]] == names(parameter_subset)[parameter_subset_i] &
                                  data_set[[parameter_header]] <= tmp_segment_threshold),]
          }
        }
      }else{

        if(!is.null(parameter_type) || parameter_type == 'duration'){
          lower_boundary = parameter_subset[[parameter_subset_i]][1] / 1000
          upper_boundary = parameter_subset[[parameter_subset_i]][2] / 1000
        }else{
          lower_boundary = parameter_subset[[parameter_subset_i]][1]
          upper_boundary = parameter_subset[[parameter_subset_i]][2]
        }

        if(!is.null(parameter_subset_header)){
          data_set = data_set[!(data_set[[parameter_subset_header]] == names(parameter_subset)[parameter_subset_i] &
                                (data_set[[parameter_header]] < lower_boundary | data_set[[parameter_header]] > upper_boundary)),]
        }
      }
    }
  }

  if(delete_uneven == T & !is.null(delete_uneven_columns)){
    #gets the count for each token
    tmp_count = count(data_set, c(delete_uneven_columns[1]))

    #gets uneven tokens
    tmp_token_number = tmp_count[tmp_count$freq != max(data_set[[delete_uneven_columns[2]]]),c(delete_uneven_columns[1])]

    #deletes uneve tokens
    data_set = data_set[!(data_set[[delete_uneven_columns[1]]] %in% tmp_token_number),]
  }

  return(data_set)
}
