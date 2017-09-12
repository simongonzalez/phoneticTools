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
  
  #argument check====================================================================================================
  
  assertDataFrame(data_set)
  
  expect_character(column_header, len = 1)
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label in column_header.")
  
  if(testNull(keep_list) & testNull(omit_list))
    stop("No values entered in either keep_list or omit_list")
  
  if(!testNull(keep_list) & length(which(keep_list %in% unique(data_set[[column_header]]))) == 0)
    stop("One or any of the values entered in keep_list does not exist in column_header values.")
  
  if(!testNull(omit_list) & length(which(omit_list %in% unique(data_set[[column_header]]))) == 0)
    stop("One or any of the values entered in omit_list does not exist in column_header values.")
  
  if(!testNull(keep_list) & length(keep_list) > length(unique(data_set[[column_header]])))
    stop("The number of values entered in keep_list is larger than the existing values in column_header.")
  
  if(!testNull(omit_list) & length(omit_list) > length(unique(data_set[[column_header]])))
    stop("The number of values entered in omit_list is larger than the existing values in column_header.")

  #keep words====================================================================================
  #==============================================================================================
  #==============================================================================================
  if(!is.null(keep_list)){
    data_set <- data_set[which(data_set[[column_header]] %in% keep_list),]
  }

  #omit words====================================================================================
  #==============================================================================================
  #==============================================================================================
  if(!is.null(omit_list)){
    data_set <- data_set[-which(data_set[[column_header]] %in% omit_list),]
  }

  return(data_set)
}
