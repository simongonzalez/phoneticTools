#' folder_content %% ~~function to do ... ~~
#' 
#' Gets the folder content of the location specified
#' 
#' Output:
#' 
#' List() containing the path and names of the files in the folder
#' 
#' @aliases folder_content
#' @param folder folder location
#' @param format_type specifies whether the files should be separated or not in
#' the list()
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
#' get_folder_content = folder_content('data', c('wav', 'tg'), separate_types = F)
#' 
#' 
folder_content <- function(folder, format_type = 'all_formats', separate_types = F){
  #check if the folder exists
  if(length(list.files(paste0(folder)) > 0)){
    if(length(format_type) == 1 & length(which(format_type %in% c('all_formats'))) == 1){
      format_type_length = 3
      format_type_label = c('.wav', '.TextGrid', '.csv')
      files_list = list.files(paste0(folder))
    }else if(length(format_type) == 1 & length(which(format_type %in% c('wav'))) == 1){
      format_type_length = 1
      format_type_label = '.wav'
      files_list = list.files(paste0(folder), pattern = '.wav')
    }else if(length(format_type) == 1 & length(which(format_type %in% c('tg'))) == 1){
      format_type_length = 1
      format_type_label = '.TextGrid'
      files_list = list.files(paste0(folder), pattern = '.TextGrid')
    }else if(length(format_type) == 1 & length(which(format_type %in% c('csv'))) == 1){
      format_type_length = 1
      format_type_label = '.csv'
      files_list = list.files(paste0(folder), pattern = '.csv')
    }else if(length(format_type) == 2 & length(which(format_type %in% c('wav', 'tg'))) == 2){
      format_type_length = 2
      format_type_label = c('.wav', '.TextGrid')
      files_list = list.files(paste0(folder), pattern = c('.wav|.TextGrid'))
    }else if(length(format_type) == 2 & length(which(format_type %in% c('wav', 'csv'))) == 2){
      format_type_length = 2
      format_type_label = c('.wav', '.csv')
      files_list = list.files(paste0(folder), pattern = c('.wav|.csv'))
    }else if(length(format_type) == 2 & length(which(format_type %in% c('tg', 'csv'))) == 2){
      format_type_length = 2
      format_type_label = c('.TextGrid', '.csv')
      files_list = list.files(paste0(folder), pattern = c('.TextGrid|.csv'))

    }else{
      files_list = 0
    }

    if(separate_types == F | format_type_length == 1){
      files_list = list(folder_files = files_list, folder_path = paste0(folder, '/'))
      return(files_list)
    }else if(separate_types == T){
      if(format_type_length == 2){
        separated_list = list(files_list[grep(format_type_label[1], files_list)],
                              files_list[grep(format_type_label[2], files_list)])
        names(separated_list) = c(gsub("\\.", "", format_type_label[1]),
                                  gsub("\\.", "", format_type_label[2]))
      }else if(format_type_length == 3){
        separated_list = list(files_list[grep(format_type_label[1], files_list)],
                              files_list[grep(format_type_label[2], files_list)],
                              files_list[grep(format_type_label[3], files_list)])
        names(separated_list) = c(gsub("\\.", "", format_type_label[1]),
                                  gsub("\\.", "", format_type_label[2]),
                                  gsub("\\.", "", format_type_label[3]))
      }

      separated_list$folder_path = paste0(folder, '/')
      return(separated_list)

    }else{
      warning('The separate_types parameter is not recognised.', call. = FALSE)
    }

  }else{
    warning('The folder is empty or does not exist.', call. = FALSE)
  }
}
