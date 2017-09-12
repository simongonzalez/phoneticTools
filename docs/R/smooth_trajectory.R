#' smooth_trajectory %% ~~function to do ... ~~
#' 
#' Smooths Trajectories
#' 
#' Output:
#' 
#' Vector list containing the segments specified
#' 
#' @aliases smooth_trajectory
#' @param data_set dataset (data.frame) containing the values to be smoothed
#' @param set_columns character vector containing the header of the columns
#' containing the values to be smoothed
#' @param smooth_method smooth method to be applied, dct or ss
#' @param smooth_parameter depending on the smooth method, the smooth parameter
#' specifies the amount of smoothness
#' @param points_number number of points in the final trajectory
#' @param even_number applies only to entries with even numbers
#' @param by_column specifies the column with the (time) points
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
#' smoothed_formants = smooth_trajectory(data_set = input_data,
#'                                       set_columns = c('f1', 'f2'), smooth_method = 'dct',
#'                                       smooth_parameter = 3, by_column = 'extraction_param')
#' 
#' 

smooth_trajectory <- function(data_set = NULL, column_header = NULL, smooth_method = NULL, smooth_parameter = NULL,
                              points_number = NULL, even_number = T, by_column = NULL){
  
  #argument check====================================================================================================
  assertDataFrame(data_set)
  
  expect_character(column_header, min.len = 1, max.len = length(colnames(data_set)))
  if(!testSubset(column_header, colnames(data_set)))
    stop("No columns in data_set with the specified label(s) in column_header.")
  
  expect_character(smooth_method, len = 1)
  if(!testSubset(smooth_method, c('dct', 'ss')))
    stop("No method with the label specified in smooth_method. Accepted labels: dct (Discrete Cosine Transform), or ss (smooth spline).")
  
  expect_number(smooth_parameter)
  if(smooth_method == 'dct' & smooth_parameter < 1)
    stop("If smooth_parameter == 'dct', smooth_parameter must be >= 1")
  
  if(smooth_method == 'ss' & smooth_parameter > 1)
    stop("If smooth_parameter == 'ss', smooth_parameter must be <= 1")
  
  if(is.null(points_number) & is.null(by_column))
    stop("Both points_number and by_column == NULL. Please fill at least one.")
  
  if(!is.null(points_number) & !is.null(by_column))
    stop("Both points_number and by_column have been filled. Please fill one or the other.")
  
  if(!is.null(points_number) & is.null(by_column))
    expect_number(points_number, lower = 2)
  
  if(is.null(points_number) & !is.null(by_column))
    expect_character(by_column, min.len = 1, max.len = length(colnames(data_set)))
  if(!testSubset(by_column, colnames(data_set)))
    stop("No columns in data_set with the specified label(s) in by_column.")
  
  for(i in 1:length(column_header)){
    
    #subset data frame
    tmp_df <- data_set[[column_header[i]]]
    #setup matrix
    if(is.null(by_column) && !is.null(points_number)){
      tmp_matrix <- matrix(tmp_df, ncol = length(tmp_df)/points_number)
    }else if(!is.null(by_column) && is.null(points_number)){
      tmp_matrix <- matrix(tmp_df, ncol = length(tmp_df)/max(data_set[[by_column]]))
    }
    
    if(smooth_method == 'dct'){
      
      if(smooth_parameter > 0){
        dfi <- apply(tmp_matrix, 2, function(x) dct(x, m = smooth_parameter, fit=TRUE))
      }else if(smooth_parameter < 0){
        dfi <- apply(tmp_matrix, 2, function(x) dct(x, m = points_number + smooth_parameter, fit=TRUE))
      }
      
    }else if(smooth_method == 'ss'){
      dfi <- apply(tmp_matrix, 2, function(x) smooth.spline(x, spar = smooth_parameter)[2]$y)
    }
    
    smooth_name <- paste0(column_header[i], '_', smooth_method)
    
    data_set[[smooth_name]] <- as.vector(dfi)
  }
  
  return(data_set)
  
}
