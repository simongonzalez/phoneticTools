#' smooth_trajectory %% ~~function to do ... ~~
#' 
#' Smooths Trajectories
#' 
#' Output:
#' 
#' Vector list containing the segments specified
#' 
#' @aliases smooth_trajectory
#' @param data_frame dataset (data.frame) containing the values to be smoothed
#' @param set_columns character vector containing the header of the columns
#' containing the values to be smoothed
#' @param smooth_method smooth method to be applied
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
#' smoothed_formants = smooth_trajectory(data_frame = input_data,
#'                                       set_columns = c('f1', 'f2'), smooth_method = 'dct',
#'                                       smooth_parameter = 3, by_column = 'extraction_param')
#' 
#' 

smooth_trajectory <- function(data_frame = NULL, set_columns = NULL, smooth_method = NULL, smooth_parameter = NULL,
                             points_number = NULL, even_number = T, by_column = NULL){

  for(i in 1:length(set_columns)){

    #subset data frame
    tmp_df = data_frame[[set_columns[i]]]
    #setup matrix
    if(is.null(by_column) && !is.null(points_number)){
      tmp_matrix = matrix(tmp_df, ncol = length(tmp_df)/points_number)
    }else if(!is.null(by_column) && is.null(points_number)){
      tmp_matrix = matrix(tmp_df, ncol = length(tmp_df)/max(data_frame[[by_column]]))
    }


    if(smooth_method == 'dct'){

      if(smooth_parameter > 0){
        df = apply(tmp_matrix, 2, function(x) dct(x, m = smooth_parameter, fit=TRUE))
      }else if(smooth_parameter < 0){
        df = apply(tmp_matrix, 2, function(x) dct(x, m = points_number + smooth_parameter, fit=TRUE))
      }

    }else if(smooth_method == 'ss'){
      df = apply(tmp_matrix, 2, function(x) smooth.spline(x, spar = smooth_parameter)[2]$y)
    }

    smooth_name = paste0(set_columns[i], '_', smooth_method)

    data_frame[[smooth_name]] = as.vector(df)
  }

  return(data_frame)

}
