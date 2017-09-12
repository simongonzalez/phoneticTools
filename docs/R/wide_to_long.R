#' wide_to_long %% ~~function to do ... ~~
#' 
#' Converts wide to long data
#' 
#' Output:
#' 
#' Exports the same dataset with the new columns
#' 
#' @aliases wide_to_long
#' @param data_set dataset (data.frame) to convert from wide to long format
#' @param column_header character vector containing the header of the columns to
#' be merged into one column
#' @param new_column_names character vector containing the names of the new
#' columns
#' @param points_column character vector containing the name of the points
#' column
#' @param points_labels character vector containing the name of the points in
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
#' long_data = wide_to_long(data_set = input_data,
#'                          column_header = 'F1.35. F1.65.,F2.35. F2.65.',
#'                          new_column_names = 'F1s F2s')
#' 
#' 

wide_to_long <- function(data_set = NULL, column_header = NULL, new_column = NULL, points_column = NULL){
  
  #argument check====================================================================================================
  
  assertDataFrame(data_set)
  
  expect_character(column_header, min.len = 1, max.len = length(colnames(data_set)))
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label(s) in column_header.")
  
  tmp_col_names <- names(data_set)
  
  expect_character(new_column, len = 1)
  existing_name <- tmp_col_names[which(tmp_col_names %in% new_column)]
  if(length(existing_name) != 0)
    stop("The label entered in new_column already exists in the data_set.")
  
  expect_character(points_column, len = 1)
  existing_name <- tmp_col_names[which(tmp_col_names %in% points_column)]
  if(length(existing_name) != 0)
    stop("The label entered in points_column already exists in the data_set.")
  
  tmp_matrix <- matrix(nrow = nrow(data_set), ncol = length(column_header))
  
  for(column_header_i in 1:length(column_header)){
    tmp_matrix[,column_header_i] <- data_set[[column_header[[column_header_i]]]]
  }
  
  tmp_vector <- as.vector(t(tmp_matrix))
  
  repeat_rows_number <- length(column_header)

  #delete the columns changed in the dataframe
  data_set <- data_set[ , !(names(data_set) %in% column_header)]

  data_set <- data_set[rep(seq_len(nrow(data_set)), each=repeat_rows_number),]

  #add the new column with values
  data_set[[new_column]] <- tmp_vector
  
  data_set[[points_column]] <- column_header

  return(data_set)
}
