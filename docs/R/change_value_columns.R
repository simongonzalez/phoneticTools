#' change_value_columns %% ~~function to do ... ~~
#' 
#' Changes column values based on user input parameters
#' 
#' Output:
#' 
#' Exports the same dataset with the values changed
#' 
#' @aliases change_value_columns
#' @param data_set dataset (data.frame) containing the values to be changed
#' @param column_header character vector containing the header of the column
#' with the values to be changed
#' @param current_values target values
#' @param new_values new values
#' @param override_column_values boolean: if TRUE, the new values overwrite the
#' old labels in the same column.
#' @param new_column_header if override_column_values == FALSE, a new column is
#' created with the specified label
#' @param delete_column_header specifies the header of the column to be deleted
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
#' new_classification = classify_segments(data_set = input_data,
#'                      column_header = 'previous_segment',
#'                      criteria = c('phonological_place', 'phonetic_place', 'phonetic_manner'),
#'                      transcription_type = 'maus')
#' 
#' 
change_value_columns <- function(data_set = NULL, column_header = NULL,
                                old_values = NULL, new_values = NULL, new_column_header = NULL,
                                delete_old_column = T){
  
  #argument check====================================================================================================
  
  assertDataFrame(data_set)
  
  expect_character(column_header, len = 1)
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label in column_header.")
  
  if(testNull(old_values))
    stop("No values entered in old_values")
  
  if(length(old_values) > length(unique(data_set[[column_header]])))
    stop("The number of values to be changed is larger than the existing values.")
  
  if(testNull(new_values))
    stop("No values entered in new_values")
  
  if(length(new_values) > length(old_values))
    stop("The number of new values is larger than the existing values.")
  
  if(length(new_values) < length(old_values) & length(new_values) > 1)
    stop("The number of old values is larger than new values.")
  
  if (!testNull(new_column_header)){
    expect_character(new_column_header, len = 1)
    if(testSubset(new_column_header, colnames(data_set)))
      stop("The label provider in new_column_header already exists. Please provide another name.")
  }
    
  if (testNull(new_column_header) & delete_old_column)
    stop("If delete_old_column == TRUE, porvide new_column_header.")
  
  
  unique_dataset_values <- as.character(sort(unique(data_set[[column_header]])))
  old_values <- sort(old_values)

  #if the input values are the same as the current values
  if(identical(unique_dataset_values, old_values)){
    if(length(old_values) == length(new_values)){
      map_values <- setNames(new_values, old_values)
    }else if(length(new_values) == 1 & length(old_values) > 1){
      new_values <- rep(new_values, length(old_values))
      map_values <- setNames(new_values, old_values)
    }
    
  }else{
    unique_dataset_values <- unique_dataset_values[!(unique_dataset_values %in% old_values)]
    
    if(length(old_values) == length(new_values)){
      #extract the mapping values from the unique dataset values
      new_values <- c(new_values, unique_dataset_values)
      old_values <- c(old_values, unique_dataset_values)
    }else if(length(new_values) == 1 & length(old_values) > 1){
      #extract the mapping values from the unique dataset values
      new_values <- c(rep(new_values, length(old_values)), unique_dataset_values)
      old_values <- c(old_values, unique_dataset_values)
    }
    
    map_values <- setNames(new_values, old_values)
    
  }
  
  if(is.null(new_column_header)){
    data_set[[column_header]] <- map_values[as.character(data_set[[column_header]])]
  }else{
    data_set[[new_column_header]] <- map_values[as.character(data_set[[column_header]])]
  }

  if(delete_old_column == T){
    data_set[[column_header]] <- NULL
  }

  return(data_set)
}
