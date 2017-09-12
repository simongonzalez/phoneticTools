#' long_to_wide %% ~~function to do ... ~~
#' 
#' Converts long to wide data
#' 
#' Output:
#' 
#' Exports the same dataset with the new columns
#' 
#' @aliases long_to_wide
#' @param data_frame dataset (data.frame) to convert from long to wide format
#' @param change_columns character vector containing the header of the columns to
#' be split into different columns
#' @param new_column_names character vector containing the names of the new
#' columns
#' @param points_column character vector containing the name of the points
#' column
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
#'                          change_columns = 'F1 F2', points_column = 'extraction_param')
#' 
#' 
long_to_wide <- function(data_set = NULL, change_columns = NULL, points_column = NULL){

  #argument check====================================================================================================
  
  assertDataFrame(data_set)
  
  expect_character(change_columns, min.len = 1, max.len = length(colnames(data_set)))
  if(!testSubset(change_columns, colnames(data_set)))
    stop("No columns in data_set with the specified label(s) in change_columns.")
  
  expect_character(points_column, len = 1)
  if(!testSubset(points_column, colnames(data_set)))
    stop("No columns in data_set with the specified label in points_column.")
  
  #stores the column headers to be deleted
  drop_names <- c(change_columns, points_column)

  #get the unique labels in the points column
  unique_tmp_labels <- unique(data_set[[points_column]])

  #creates the column headers of the new dataframe
  new_column_header_merge <- gsub(" ", '_', c(t(outer(change_columns, unique_tmp_labels, paste))))

  #counter for storing in the new wide dataframe
  new_column_counter <- 1

  #iteration for every column
  for(merge_i in 1:length(change_columns)){

    #creates a dataframe that stores temporal values for the new columns
    if(merge_i == 1){
      tmp_new_columns <- data.frame(matrix(nrow = (nrow(data_set)/length(unique_tmp_labels)),
                                          ncol = length(new_column_header_merge)))
      names(tmp_new_columns) <- new_column_header_merge
    }

    #iterates to collect the subsetting values for each column
    for(point_i in unique_tmp_labels){

      tmp_new_columns[[new_column_counter]] <- data_set[data_set[[points_column]] == point_i, change_columns[merge_i]]
      new_column_counter <- new_column_counter + 1
    }

  }

  #delete the columns of the current dataframe
  data_set <- data_set[ , !(names(data_set) %in% drop_names)]

  #gets one value per point
  data_set <- data_set[seq(1, NROW(data_set), by = length(unique_tmp_labels)),]

  new_d <- cbind(data_set, tmp_new_columns)

  return(new_d)
}
