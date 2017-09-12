#' filter_strings %% ~~function to do ... ~~
#' 
#' Filters strings based on user input parameters
#' 
#' Output:
#' 
#' Exports the same dataset with the same number of columns containing the
#' filtered elements
#' 
#' @aliases filter_strings
#' @param data_set dataset (data.frame) containing the strings to be filtered
#' @param column_header character vector containing the header of the column
#' containing the strings to be filtered
#' @param orthography_type contains = partial/total matching - all words
#' containing x
#' 
#' begins = partial matching - all words begining with x
#' 
#' ends = partial matching - all words ending with x
#' @param orthography_filter pattern to be filtered
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
#' new_filtering = filter_strings(data_set = input_data,
#'                                column_header = 'phrase',
#'                                orthography_type = 'contains',
#'                                orthography_filter = c('tev'))
#' 
filter_strings <- function(data_set = NULL, column_header = NULL,
                         orthography_type = NULL, orthography_filter = NULL){
  
  #argument check====================================================================================================

  assertDataFrame(data_set)
  
  expect_character(column_header, len = 1)
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label in column_header.")
  
  if(testNull(orthography_type))
    stop("No values entered in orthography_type")
  
  if(testNull(orthography_filter))
    stop("No values entered in orthography_filter")
  #orthography===============================================================
  #==========================================================================
  #==========================================================================
  if(!is.null(orthography_type)){

    if(orthography_type == 'contains'){
      orthography_list <- paste0('(', paste(orthography_filter, collapse = '|'), ')')
    }else if(orthography_type == 'begins'){
      orthography_list <- paste0('^(', paste(orthography_filter, collapse = '|'), ')')
    }else if(orthography_type == 'ends'){
      orthography_list <- paste0('(', paste(orthography_filter, collapse = '|'), ')$')
    }

    data_set <- data_set[grepl(orthography_list, data_set[[column_header]]),]

    return(data_set)
  }

}
