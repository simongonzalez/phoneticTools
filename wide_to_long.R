#' wide_to_long %% ~~function to do ... ~~
#' 
#' Converts wide to long data
#' 
#' Output:
#' 
#' Exports the same dataset with the new columns
#' 
#' @aliases wide_to_long
#' @param data_frame dataset (data.frame) to convert from wide to long format
#' @param set_columns character vector containing the header of the columns to
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
#'                          set_columns = 'F1.35. F1.65.,F2.35. F2.65.',
#'                          new_column_names = 'F1s F2s')
#' 
#' 

wide_to_long <- function(data_frame = NULL, set_columns = NULL, new_column_names = NULL,
                        points_column = NULL, points_labels = NULL){

  split_new_column_names = unlist(strsplit(new_column_names, " "))

  split_set_columns = unlist(strsplit(set_columns, ","))

  drop_names = c()

  for(merge_i in 1:length(split_set_columns)){
    tmp_merge_string = split_set_columns[merge_i]

    split_set_columns_i = unlist(strsplit(tmp_merge_string, " "))

    if(merge_i == 1){
      tmp_new_columns = data.frame(matrix(nrow = (nrow(data_frame)*length(split_set_columns_i)),
                                          ncol = length(split_set_columns)))
      names(tmp_new_columns) = split_new_column_names

      repeat_rows_number = length(split_set_columns_i)
    }

    tmp_matrix = matrix(nrow = nrow(data_frame), ncol = length(split_set_columns_i))

    for(set_columns_i in 1:length(split_set_columns_i)){
      tmp_matrix[,set_columns_i] = data_frame[[split_set_columns_i[set_columns_i]]]
    }
    tmp_new_columns[[split_new_column_names[merge_i]]] = as.vector(t(tmp_matrix))

    drop_names = c(drop_names, split_set_columns_i)
  }

  #delete the columns changed in the dataframe
  df = data_frame[ , !(names(data_frame) %in% drop_names)]

  new_d = df[rep(seq_len(nrow(df)), each=repeat_rows_number),]

  #add the new columns
  for(i in split_new_column_names){
    new_d[[i]] = tmp_new_columns[[i]]
  }

  split_points_labels = unlist(strsplit(points_labels, " "))

  new_d[[points_column]] = split_points_labels

  return(new_d)
}
