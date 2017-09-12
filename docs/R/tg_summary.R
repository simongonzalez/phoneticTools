#' tg_summary %% ~~function to do ... ~~
#' 
#' Gives TextGrid summary
#' 
#' Output:
#' 
#' List containing the summary of the TextGrids
#' 
#' @aliases tg_summary
#' @param incoming_list List containing the path and TextGrid names
#' @param interval_label Specifies which labels will be summarised
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
#' TextGrid_summary = tg_summary(fl_lst)
#' 
#' 
#' 
tg_summary <- function(incoming_list = NULL, interval_label = 'all_labels'){
  
  #argument check====================================================================================================
  
  assertList(incoming_list)
  
  expect_character(interval_label, min.len = 1)

  dirs <- paste0(getwd(), '/', incoming_list$folder_path)
  FullPath <- function(FileName){ return( paste( dirs, FileName, sep="") ) }

  #test whether files have been separated by type
  tmp_names <- names(incoming_list)

  #if files are not separated by type
  if(length(which('folder_files' %in% tmp_names)) > 0){
    tgs <- incoming_list$folder_files[grep('.TextGrid', incoming_list$folder_files)]
  }else{
    tgs <- incoming_list$TextGrid
  }

  tg_main_list <- c()

  for(tg_i in 1:length(tgs)){
    tg <- tgs[tg_i]
    #get time information
    initial_time <- as.numeric(praat('Get start time', input=FullPath(tg), simplify=TRUE))
    end_time <- as.numeric(praat('Get end time', input=FullPath(tg), simplify=TRUE))
    total_duration <- as.numeric(praat('Get total duration', input=FullPath(tg), simplify=TRUE))

    #get number of tiers
    number_of_tiers <- as.numeric(praat('Get number of tiers', input=FullPath(tg), simplify=TRUE))


    tier_names <- c()

    #get name of tiers
    for(i in 1:number_of_tiers){
      tier_names[i] <- praat('Get tier name...', list(i), input=FullPath(tg))
    }

    #get number of intervals per tier
    number_of_intervals <- c()

    for(i in 1:number_of_tiers){
      number_of_intervals[i] <- as.numeric(praat('Get number of intervals...', list(i), input=FullPath(tg), simplify = T))
    }

    #number of not-empty intervals
    number_of_intervals_not_empty <- c()
    for(i in 1:number_of_tiers){
      not_empty_counter <- 0

      for(j in 1:number_of_intervals[i]){
        tmp_label <- praat('Get label of interval...', list(i, j), input=FullPath(tg))

        if(interval_label == 'all_labels' & tmp_label != ''){
          not_empty_counter <- not_empty_counter + 1
        }else if(interval_label != 'all_labels' & length(which(tmp_label %in% interval_label)) > 0){
          not_empty_counter <- not_empty_counter + 1
        }
      }
      number_of_intervals_not_empty[i] <- not_empty_counter
    }

    #convert all NA values to 0
    number_of_intervals_not_empty <- replace(number_of_intervals_not_empty,which(is.na(number_of_intervals_not_empty)),0)

    #number of empty intervals
    #number of not-empty intervals
    number_of_intervals_empty <- c()
    for(i in 1:number_of_tiers){
      empty_counter <- 0

      for(j in 1:number_of_intervals[i]){
        tmp_label <- praat('Get label of interval...', list(i, j), input=FullPath(tg))

        if(tmp_label == ''){
          empty_counter <- empty_counter + 1
        }
      }
      number_of_intervals_empty[i] <- empty_counter
    }

    time_info_list <- list(initial_time = initial_time, end_time = end_time, total_duration = total_duration)
    tier_info_list <- list(number_of_tiers = number_of_tiers, tier_names = tier_names)
    interval_info_list <- list(interval_label = interval_label, number_of_intervals = number_of_intervals,
                              number_of_intervals_not_empty = number_of_intervals_not_empty,
                              number_of_intervals_empty = number_of_intervals_empty)

    tg_list <- list(tg_name = tg, time_info = time_info_list, tier_info = tier_info_list, interval_info = interval_info_list)

    tg_main_list[[paste0('tg_', tg_i)]] <- tg_list
  }
  return(tg_main_list)
}

