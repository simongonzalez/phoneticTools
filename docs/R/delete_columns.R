#' delete_columns %% ~~function to do ... ~~
#' 
#' Deletes the column(s) specified
#' 
#' Output:
#' 
#' Exports the same dataset with the columns deleted
#' 
#' @aliases delete_columns
#' @param data_set dataset (data.frame) containing the column(s) to be deleted
#' @param delete_column character vector containing the header(s) of the
#' column(s) to be deleted
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
#' new_classification = classify_segments(data_set = input_data,
#'                      column_header = 'previous_segment',
#'                      criteria = c('phonological_place', 'phonetic_place', 'phonetic_manner'),
#'                      transcription_type = 'maus')
#' 
#' 
delete_columns <- function(data_set = NULL, delete_column = NULL){
  
  #argument check====================================================================================================
  
  assertDataFrame(data_set)
  
  expect_character(delete_column, min.len = 1, max.len = ncol(data_set))
  if(!testSubset(delete_column, colnames(data_set)))
    stop("No columns in data_set with all or any of the specified labels in delete_column.")

  for(i in delete_column){
    data_set[[i]] <- NULL
  }

  return(data_set)
}
