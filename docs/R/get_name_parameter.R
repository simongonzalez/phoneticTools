#' get_name_parameter %% ~~function to do ... ~~
#' 
#' Gets the data specifications from a string name
#' 
#' Output:
#' 
#' List() containing the path and names of the files in the folder
#' 
#' @aliases get_name_parameter
#' @param complete_name full string containing the parameters to be extracted
#' @param name_separator separator character in the string
#' @param specify_location specifies the location of the parameters to be
#' extracted
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
#' string_section = get_name_parameter('speaker_JaneDoe_f_DoverSchool_sample', '_', c(2,3))
#' 
get_name_parameter <- function(complete_name = NULL, name_separator = NULL, specify_location = NULL){
  
  #argument check====================================================================================================

  expect_character(complete_name, len = 1)
  
  expect_character(name_separator, len = 1)
  
  expect_number(specify_location, lower = 1)

  #get location of all separators
  separator_locs <- unlist(lapply(strsplit(complete_name, ''), function(x) which(x == name_separator)))
 
  if(separator_locs[length(separator_locs)] == nchar(complete_name))
    stop("name_separator found at the end of complete_name.")
  
  if(length(separator_locs) == 0)
    stop("No name_separator found in complete_name.")
  
  if(specify_location > (length(separator_locs)+1))
    stop("specify_location is larger than number of sections.")
  
  locs_matrix <- matrix(nrow = (length(separator_locs)+1), ncol = 2)

  locs_matrix[1,1] <- 1
  locs_matrix[1,2] <- separator_locs[1]-1
  
  if(length(separator_locs) != 1){
    for(i in 2:(nrow(locs_matrix)-1)){
      locs_matrix[i,1] <- separator_locs[i-1]+1
      locs_matrix[i,2] <- separator_locs[i]-1
    }
  }
  
  locs_matrix[nrow(locs_matrix),1] <- separator_locs[length(separator_locs)]+1
  locs_matrix[nrow(locs_matrix),2] <- nchar(complete_name)

  output_parameter <- substr(complete_name, locs_matrix[specify_location,1], locs_matrix[specify_location,2])

  return(output_parameter)
}
