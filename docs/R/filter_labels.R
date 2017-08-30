#' filter_labels %% ~~function to do ... ~~
#' 
#' Filters labels of any column based on user input parameters
#' 
#' Output:
#' 
#' Exports the same dataset with the same number of columns containing the
#' filtered elements
#' 
#' @aliases filter_labels
#' @param data_set dataset (data.frame) containing the columns to be filtered
#' @param column_header character variable containing the header of the column
#' to have content filtered
#' @param keep_list character vector containing the elements to be kept in the
#' column
#' @param omit_list character vector containing the elements to be left out in
#' the column
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
#' new_filtering = filter_labels(data_set = input_data,
#'                 column_header = 'segment',
#'                 keep_list = c('i:', 'u', 'U'),
#'                 omit_list = NULL)
#' 
#' 
filter_labels <- function(data_set = NULL, column_header = NULL,
                         keep_list = NULL, omit_list = NULL){

  #keep words====================================================================================
  #==============================================================================================
  #==============================================================================================
  if(!is.null(keep_list)){
    data_set = data_set[which(data_set[[column_header]] %in% keep_list),]
  }

  #omit words====================================================================================
  #==============================================================================================
  #==============================================================================================
  if(!is.null(omit_list)){
    data_set = data_set[-which(data_set[[column_header]] %in% omit_list),]
  }

  return(data_set)
}
