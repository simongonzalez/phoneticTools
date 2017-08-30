#' filter_word_syllables %% ~~function to do ... ~~
#' 
#' Filters words based on number of syllables
#' 
#' Output:
#' 
#' Exports the same dataset with the same number of columns containing the
#' filtered elements
#' 
#' @aliases filter_word_syllables
#' @param data_set dataset (data.frame) containing the words to be filtered
#' @param column_header character vector containing the header of the column
#' containing the words to be filtered
#' @param number_of_syllables number of syllables to be filtered
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
#' new_filtering = filter_word_syllables(data_set = input_data,
#'                                       column_header = 'word',
#'                                       number_of_syllables = c(2,3))
#' 
#' 
filter_word_syllables <- function(data_set = NULL, column_header = NULL, number_of_syllables = NULL){

  #number of syllables======================================================================
  #=========================================================================================
  #=========================================================================================
  if(!is.null(number_of_syllables)){

    if(is.character(data_set[[column_header]])){
      if(length(number_of_syllables) == 1){
        if(is.numeric(number_of_syllables)){
          data_set = data_set[unlist(lapply(data_set[[column_header]], function(x) syllable_count(x)$syllables == number_of_syllables)),]
        }else{
          if(number_of_syllables == 'mono'){
            data_set = data_set[unlist(lapply(data_set[[column_header]], function(x) syllable_count(x)$syllables == 1)),]
          }else if(number_of_syllables == 'multi'){
            data_set = data_set[unlist(lapply(data_set[[column_header]], function(x) syllable_count(x)$syllables != 1)),]
          }
        }
      }else{
        if(number_of_syllables[1] < 0){
          data_set = data_set[unlist(lapply(data_set[[column_header]], function(x) syllable_count(x)$syllables >= (number_of_syllables[1] * -1) &
                                              syllable_count(x)$syllables <= number_of_syllables[2])),]
        }else{
          data_set = data_set[unlist(lapply(data_set[[column_header]], function(x) syllable_count(x)$syllables == number_of_syllables[1] ||
                                              syllable_count(x)$syllables == number_of_syllables[2])),]
        }
      }
    }else if(is.numeric(data_set[[column_header]])){
      if(length(number_of_syllables) == 1){
        if(is.numeric(number_of_syllables)){
          data_set = data_set[data_set[[column_header]] == number_of_syllables,]
        }else{
          if(number_of_syllables == 'mono'){
            data_set = data_set[data_set[[column_header]] == 1,]
          }else if(number_of_syllables == 'multi'){
            data_set = data_set[data_set[[column_header]] != 1,]
          }
        }
      }else{
        if(number_of_syllables[1] < 0){
          data_set = data_set[data_set[[column_header]] >= (number_of_syllables[1] * -1) & data_set[[column_header]] <= number_of_syllables[2],]
        }else{
          data_set = data_set[data_set[[column_header]] == number_of_syllables[1] || data_set[[column_header]] == number_of_syllables[2],]
        }
      }
    }

  }
  return(data_set)
}
