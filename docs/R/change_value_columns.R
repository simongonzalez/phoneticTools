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

  old_values = unlist(strsplit(old_values, " "))
  new_values = unlist(strsplit(new_values, " "))

  map_values = setNames(new_values, old_values)

  data_set[[new_column_header]] = map_values[as.character(data_set[[column_header]])]

  if(delete_old_column == T){
    data_set[[column_header]] = NULL
  }

  return(data_set)
}

#print(head(change_value_columns(d, 'segment', 'i:', 'FLEECE', 'segment', F)))
