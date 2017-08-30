#' rename_columns %% ~~function to do ... ~~
#' 
#' Renames the column(s) specified
#' 
#' Output:
#' 
#' Exports the same dataset with the columns renamed
#' 
#' @aliases rename_columns
#' @param data_set dataset (data.frame) containing the column(s) to be renamed
#' @param current_column character vector containing the header(s) of the
#' column(s) to be renamed
#' @param new_column character vector containing the name(s) of the new
#' column(s)
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
rename_columns <- function(data_set = NULL, current_column = NULL, new_column = NULL){

  current_column = unlist(strsplit(current_column, " "))

  new_column = unlist(strsplit(new_column, " "))

  for(i in 1:length(current_column)){
    colnames(data_set)[which(names(data_set) == current_column[i])] = new_column[i]
  }

  return(data_set)
}
