#' Changes labels %% ~~function to do ... ~~
#' 
#' Changes labels based on user input parameters
#' 
#' Output:
#' 
#' Exports the same dataset with labels changed
#' 
#' @aliases change_labels
#' @param data_set dataset (data.frame) containing the labels to be changed
#' @param column_header character vector containing the header of the column
#' with the labels to be changed
#' @param current_set target labels
#' @param new_set new labels
#' @param subset_segments character vector with a subset of segments, either vowels or consonants
#' @param override_column_values boolean: if TRUE, the new labels overwrite the
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
change_labels <- function(data_set = NULL, column_header = NULL,
                          current_set = NULL, new_set = NULL, subset_segments = NULL, override_column_values = TRUE,
                          new_column_header = NULL, delete_column_header = FALSE){
  
  #argument check====================================================================================================
  assertDataFrame(data_set)
  
  expect_character(column_header, len = 1)
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label in column_header.")
  
  expect_character(current_set, len = 1)
  if(!testSubset(current_set, c('celex', 'arpabet', 'disc', 'hex', 'ipa', 'maus', 'sampa', 'lexset')))
    stop("No set with the label specified in current_set. Accepted labels: celex, arpabet, disc, hex, ipa, maus, sampa, lexset.")
  
  expect_character(new_set, len = 1)
  if(!testSubset(new_set, c('celex', 'arpabet', 'disc', 'hex', 'ipa', 'maus', 'sampa', 'lexset')))
    stop("No set with the label specified in new_set. Accepted labels: celex, arpabet, disc, hex, ipa, maus, sampa, lexset.")
  
  if (!is.null(subset_segments))
    expect_character(subset_segments, info = 'Expected values: "vowels" or "consonants"')
  
  if(!testSubset(subset_segments, c('vowels', 'consonants')))
    stop("No subset with the label specified in subset_segments. Accepted labels: vowels, consonants.")
  
  if(testNull(override_column_values))
    stop("override_column_values MUST be BOOLEAN - TRUE or FALSE")
  
  if (override_column_values != FALSE & override_column_values != TRUE)
    stop("override_column_values MUST be BOOLEAN - TRUE or FALSE")
  
  if (override_column_values == FALSE & is.null(new_column_header))
    stop("Need to specify new_column_header if override_column_values == F.")
  
  if (override_column_values == FALSE & !is.null(new_column_header)  ||  override_column_values ==TRUE & !is.null(new_column_header))
    expect_character(new_column_header)
  
  if (override_column_values == TRUE & is.null(new_column_header))
    stop("If override_column_values == TRUE, porvide new_column_header.")
  
  if(testNull(delete_column_header))
    stop("override_column_values MUST be BOOLEAN - TRUE or FALSE")
  
  if (delete_column_header != FALSE & delete_column_header != TRUE | is.numeric(delete_column_header) | is.character(delete_column_header))
    stop("delete_column_header MUST be BOOLEAN - TRUE or FALSE")

  
  
  
  
  
  
  
  #subsettting the data
  if(is.null(subset_segments)){
    current_values <- as.vector(unlist(get_symbols(current_set)))
    new_values <- as.vector(unlist(get_symbols(new_set)))
  }else{
    current_values <- as.vector(unlist(get_symbols(current_set)[[subset_segments]]))
    new_values <- as.vector(unlist(get_symbols(new_set)[[subset_segments]]))
  }
  
  #creates the mapping values
  map_values <- setNames(new_values, current_values)
  
  if(!is.null(new_column_header)){
    data_set[[new_column_header]] <- map_values[as.character(data_set[[column_header]])]
  }
  
  #saves the new values to the dataframe
  if(override_column_values == T){
    data_set[[column_header]] <- map_values[as.character(data_set[[column_header]])]
  }
  

  
  if(delete_column_header){
    data_set[[column_header]] <- NULL
  }
  
  return(data_set)
  
}
