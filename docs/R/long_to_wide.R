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
#' @param set_columns character vector containing the header of the columns to
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
#'                          set_columns = 'F1.35. F1.65.,F2.35. F2.65.',
#'                          new_column_names = 'F1s F2s')
#' 
#' 
long_to_wide <- function(data_frame = NULL, set_columns = NULL, new_column_names = NULL,
                        points_column = NULL){

  #headers of columns to be changed
  split_set_columns = unlist(strsplit(set_columns, " "))

  #headers for the new columns created
  split_new_column_names = unlist(strsplit(new_column_names, " "))

  #stores the column headers to be deleted
  drop_names = c(split_set_columns, points_column)

  #get the unique labels in the points column
  unique_tmp_labels = unique(data_frame[[points_column]])

  #creates the column headers of the new dataframe
  new_column_header_merge = gsub(" ", '_', c(t(outer(split_new_column_names, unique_tmp_labels, paste))))

  #counter for storing in the new wide dataframe
  new_column_counter = 1

  #iteration for every column
  for(merge_i in 1:length(split_set_columns)){

    #creates a dataframe that stores temporal values for the new columns
    if(merge_i == 1){
      tmp_new_columns = data.frame(matrix(nrow = (nrow(data_frame)/length(unique_tmp_labels)),
                                          ncol = length(new_column_header_merge)))
      names(tmp_new_columns) = new_column_header_merge
    }

    #iterates to collect the subsetting values for each column
    for(point_i in unique_tmp_labels){

      tmp_new_columns[[new_column_counter]] = data_frame[data_frame[[points_column]] == point_i, split_set_columns[merge_i]]
      new_column_counter = new_column_counter + 1
    }

  }

  #delete the columns of the current dataframe
  df = data_frame[ , !(names(data_frame) %in% drop_names)]

  #gets one value per point
  df = df[seq(1, NROW(df), by = length(unique_tmp_labels)),]

  new_d = cbind(df, tmp_new_columns)

  return(new_d)
}
