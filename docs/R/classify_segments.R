#' classify_segments %% ~~function to do ... ~~
#' 
#' Classifies segments into phonological and phonetic features
#' 
#' Output:
#' 
#' Exports the same dataset with new columns containing the
#' phonetic/phonological classification of segments
#' 
#' @aliases classify_segments
#' @param data_set dataset (data.frame) containing the segments to be classifed
#' @param column_header character vector containing the header of the column
#' containing the segments to be classified
#' @param criteria character vector containing the phonetic/phonological
#' features to classify segments
#' @param transcription_type character variable containing the name of the
#' phonetic transcription convention used
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
#' new_classification = classify_segments(data_set = input_data,
#'                      column_header = 'previous_segment',
#'                      criteria = c('phonological_place', 'phonetic_place', 'phonetic_manner'),
#'                      transcription_type = 'maus')
#' 

classify_segments <- function(data_set = NULL, column_header = NULL,
                          criteria = NULL, transcription_type = NULL){

  #phnological features
  #consonants
  consonant_features = c('consonantal','sonorant','continuant','labial','bilabial',
                         'labiodental','anterior','coronal','distributed','dorsal',
                         'sibilant','nasal','lateral','delayed_release', 'voiced','voiceless',
                         'dental','alveolar','palatoalveolar','palatal','velar',
                         'labiovelar','glottal','stop', 'plosive','fricative',
                         'affricate','approximant','liquid','rhotic')
  full_set_consonant_features = c('phonological_place', 'phonetic_place', 'phonetic_manner')
  #vowels
  vowel_features = c('high','low','front','back','long',
                     'round','mid','central','monoth','diph', 'tense')

  full_set_vowel_features = c('phonetic_height', 'phonetic_advancement')

  print(criteria)

  for(i in 1:length(criteria)){

    tmp_criterion = criteria[i]

    if(length(which(tmp_criterion %in% full_set_consonant_features)) != 0 ||
       length(which(tmp_criterion %in% full_set_vowel_features)) != 0){

      if(length(which(tmp_criterion %in% full_set_consonant_features)) != 0){

        tmp_segments = ipacorr(transcription_type)$consonants

        #get all the segments of interest
        if(tmp_criterion == 'phonological_place'){
          tmp_labels = c('labial','coronal','dorsal', 'glottal')
        }

        if(tmp_criterion == 'phonetic_place'){
          tmp_labels = c('bilabial','labiodental','dental', 'alveolar', 'palatoalveolar',
                         'palatal', 'velar', 'labiovelar', 'glottal')
        }

        if(tmp_criterion == 'phonetic_manner'){
          tmp_labels = c('stop', 'nasal', 'fricative', 'affricate', 'approximant', 'lateral',
                         'liquid', 'rhotic')
        }

        data_set[[paste0(tmp_criterion, '_', column_header)]] = ""

        for(tmp_i in tmp_labels){
          #find the segments that meet the criteria
          tmp_segments_criteria = ipacorr(transcription_type, consonant_set = tmp_i)$consonants

          if(length(tmp_segments_criteria) != 0){
            data_set[data_set[[column_header]] %in% tmp_segments_criteria,c(paste0(tmp_criterion, '_', column_header))] = tmp_i
          }
        }

      }else if(length(which(tmp_criterion %in% full_set_vowel_features)) != 0){
        tmp_segments = ipacorr(transcription_type)$vowels

        #get all the segments of interest

        if(tmp_criterion == 'phonetic_height'){
          tmp_labels = c('high', 'mid', 'low')
        }

        if(tmp_criterion == 'phonetic_advancement'){
          tmp_labels = c('front', 'central', 'back')
        }

        data_set[[paste0(tmp_criterion, '_', column_header)]] = ""

        for(tmp_i in tmp_labels){
          #find the segments that meet the criteria
          tmp_segments_criteria = ipacorr(transcription_type, vowel_set = tmp_i)$vowels

          if(length(tmp_segments_criteria) != 0){
            data_set[data_set[[column_header]] %in% tmp_segments_criteria,c(paste0(tmp_criterion, '_', column_header))] = tmp_i
          }
        }
      }
    }else{
      if(length(which(tmp_criterion %in% consonant_features)) != 0){
        #get all the segments of interest
        tmp_segments = ipacorr(transcription_type)$consonants
        #find the segments that meet the criteria
        tmp_segments_criteria = ipacorr(transcription_type, consonant_set = tmp_criterion)$consonants
      }else if(length(which(tmp_criterion %in% vowel_features)) != 0){
        #get all the segments of interest
        tmp_segments = ipacorr(transcription_type)$vowels
        #find the segments that meet the criteria
        tmp_segments_criteria = ipacorr(transcription_type, vowel_set = tmp_criterion)$vowels
      }

      #identifies the result of the mapping
      tmp_segments_result = ifelse(tmp_segments %in% tmp_segments_criteria, 'y', 'n')

      #creates a mapping list
      segments_mapping = setNames(as.character(tmp_segments_result), as.character(tmp_segments))

      #adds the results to the dataframe
      data_set[[paste0(tmp_criterion, '_', column_header)]] = segments_mapping[as.character(data_set[[column_name]])]
    }

  }

  return(data_set)
}
