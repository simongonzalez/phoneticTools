#' get_values %% ~~function to do ... ~~
#' 
#' Gets acoustic values from wav files and their corresponding TextGrids
#' 
#' Output:
#' 
#' data.frame containing all the values specified
#' 
#' @aliases get_values
#' @param incoming_list List containing the location and names of files
#' @param data_summary List containing the summary of the TextGrids
#' @param name_separator separator to obtain parameters from file names
#' @param name_parameters location of parameters in file names
#' @param segment_tier Number of segment tier in the TextGrid
#' @param word_tier Number of word tier in the TextGrid
#' @param phrase_tier Number of phrase tier in the TextGrid
#' @param specify_segments manual segment list
#' @param add_segment segments to be added to the default list
#' @param delete_segment segments to be omitted in the default list
#' @param focus_words manual word list
#' @param omit_words words to be omitted in the default list
#' @param focus_phrases manual phrase list
#' @param omit_phrases phrases to be omitted in the default list
#' @param contiguous_segment coding contiguous segments
#' @param contiguous_word coding contiguous words
#' @param contiguous_phrase coding phrases
#' @param segment_position coding segment position in the word(initial, medial,
#' final)
#' @param word_position coding segment position in the phrase(initial, medial,
#' final)
#' @param pauses code pauses
#' @param set_contiguous_segment contiguous segment based on word
#' @param set_contiguous_word contiguous segment based on phrase
#' @param pause_marker specify pause marker
#' @param extraction type of data extraction, points, percentage, every
#' @param extraction_param number of points in the data extraction
#' @param symbol_format symbol set in which the data is coded
#' @param onset code values in the onset position of the vowel
#' @param middle code values in the middle position of the vowel
#' @param offset code values in the offset position of the vowel
#' @param mean_value code mean values of vowels
#' @param duration code vowel duration
#' @param formants code formant values of vowels
#' @param specify_formants specify which formants are to be extracted
#' @param intensity code for intensity
#' @param pitch code for pitch
#' @param nas_to_zeros convert NA values to zeros
#' @param delete_praat_files delete praat files after coding
#' @param save_as_csv save dataframe into csv
#' @param specify_folder specifies the folder where the dataframe is saved
#' @param specify_name specify the name of the file, default 'as date'
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
#' new_df = get_values(incoming_list = fl_lst, data_summary = fl_sum,
#'            name_parameters = c('speaker' = 2, 'gender' = 3, 'school' = 5, 'speechStyle' = 6),
#'            segment_tier = 3, word_tier = 2, phrase_tier = 1,
#'            specify_segments = seg_lst$vowels,
#'            specify_formants = c('f1', 'f2'), specify_folder = 'incomingdata')
#' 
get_values <- function(incoming_list = NULL, data_summary = NULL, name_separator = NULL, name_parameters = NULL,
                       segment_tier = NULL, word_tier = NULL, phrase_tier = NULL,
                       specify_segments = NULL, add_segment = NULL, delete_segment = NULL,
                       focus_words = NULL, omit_words = NULL, focus_phrases = NULL, omit_phrases = NULL,
                       contiguous_segment = NULL, contiguous_word = NULL, contiguous_phrase = NULL,
                       segment_position = TRUE, word_position = FALSE, pause_marker = NULL,
                       set_contiguous_segment = NULL, set_contiguous_word = NULL,
                       extraction = 'points', extraction_param = 5, symbol_format = 'maus',
                       onset = FALSE, middle = FALSE, offset = FALSE, mean_value = FALSE,
                       duration = FALSE, formants = TRUE, specify_formants = c('f1', 'f2'), intensity = FALSE, pitch = FALSE, nas_to_zeros = T,
                       delete_praat_files = T, save_as_csv = T, specify_folder = NULL, specify_name = 'asdate'){
  
  #argument check====================================================================================================
  
  assertList(incoming_list)
  assertList(data_summary)
  expect_character(name_separator, len = 1)
  expect_numeric(name_parameters, min.len = 1)
  
  if(!is.null(segment_tier))
    expect_number(segment_tier, lower = 1)
  
  if(!is.null(word_tier))
    expect_number(word_tier, lower = 1)
  
  if(!is.null(phrase_tier))
    expect_number(phrase_tier, lower = 1)
  
  if(is.null(segment_tier) & is.null(word_tier) & is.null(phrase_tier))
    stop("No tier selection. Please select at least segment_tier, word_tier, or phrase_tier")
  
  #------------------------------------------------------------
  
  if(!is.null(specify_segments))
    expect_character(specify_segments, min.len = 1)
  
  if(!is.null(add_segment))
    expect_character(add_segment, min.len = 1)
  
  if(!is.null(delete_segment))
    expect_character(delete_segment, min.len = 1)
  
  if(!is.null(focus_words))
    expect_character(focus_words, min.len = 1)
  
  if(!is.null(omit_words))
    expect_character(omit_words, min.len = 1)
  
  if(!is.null(focus_phrases))
    expect_character(focus_phrases, min.len = 1)
  
  if(!is.null(omit_phrases))
    expect_character(omit_phrases, min.len = 1)
  
  #------------------------------------------------------------
  if(!is.null(segment_tier)){
    if(!is.null(contiguous_segment)){
      expect_character(contiguous_segment, len = 1)
      if(!testSubset(contiguous_segment, c('all', 'prev', 'foll', 'intvoc')))
        stop("No parameter with the label specified in contiguous_segment. Accepted labels: all, prev, foll, intvoc.")
    }
    
    if (segment_position != FALSE & segment_position != TRUE)
      stop("segment_position MUST be BOOLEAN - TRUE or FALSE")
    
    if(!is.null(set_contiguous_segment)){
      expect_character(set_contiguous_segment, len = 1)
      if(!testSubset(set_contiguous_segment, c('all', 'prev', 'foll', 'intvoc')))
        stop("No parameter with the label specified in set_contiguous_segment. Accepted labels: all, prev, foll, intvoc.")
    }
    
  }
  
  if(!is.null(word_tier)){
    if(!is.null(contiguous_word)){
      expect_character(contiguous_word, len = 1)
      if(!testSubset(contiguous_word, c('all', 'prev', 'foll', 'intvoc')))
        stop("No parameter with the label specified in contiguous_word. Accepted labels: all, prev, foll, intvoc.")
    }
    
    if (word_position != FALSE & word_position != TRUE)
      stop("word_position MUST be BOOLEAN - TRUE or FALSE")
    
    if(!is.null(set_contiguous_word)){
      expect_character(set_contiguous_word, len = 1)
      if(!testSubset(set_contiguous_word, c('all', 'prev', 'foll', 'intvoc')))
        stop("No parameter with the label specified in set_contiguous_word. Accepted labels: all, prev, foll, intvoc.")
    }
  }
  
  if(!is.null(phrase_tier)){
    if(!is.null(contiguous_phrase)){
      expect_character(contiguous_phrase, len = 1)
      if(!testSubset(contiguous_phrase, c('all', 'prev', 'foll', 'intvoc')))
        stop("No parameter with the label specified in contiguous_phrase. Accepted labels: all, prev, foll, intvoc.")
    }
  }
  #------------------------------------------------------------
  if(!is.null(pause_marker))
    expect_character(pause_marker, len = 1)
  
  expect_character(extraction, len = 1)
  if(!testSubset(extraction, c('points', 'percentage', 'every')))
    stop("No parameter with the label specified in extraction. Accepted labels: points, percentage, every.")
  
  if(extraction == 'percentage'){
    expect_numeric(extraction_param, min.len = 1)
  }else{
    expect_number(extraction_param, lower = 1)
  }
  
  set_ids <- sort(c('celex', 'arpabet', 'disc', 'hex', 'ipa', 'maus', 'sampa', 'lexset'))
  
  expect_character(symbol_format, len = 1)
  if(!testSubset(symbol_format, set_ids))
    stop(paste0("No format with the label specified in symbol_format. Accepted labels: ", paste0(set_ids, collapse = ', '), "."))
  
  #------------------------------------------------------------
  
  if (onset != FALSE & onset != TRUE)
    stop("onset MUST be BOOLEAN - TRUE or FALSE")
  
  if (middle != FALSE & middle != TRUE)
    stop("middle MUST be BOOLEAN - TRUE or FALSE")
  
  if (offset != FALSE & offset != TRUE)
    stop("offset MUST be BOOLEAN - TRUE or FALSE")
  
  if (mean_value != FALSE & mean_value != TRUE)
    stop("mean_value MUST be BOOLEAN - TRUE or FALSE")
  
  if (duration != FALSE & duration != TRUE)
    stop("duration MUST be BOOLEAN - TRUE or FALSE")
  
  if (formants != FALSE & formants != TRUE)
    stop("formants MUST be BOOLEAN - TRUE or FALSE")
  
  expect_character(specify_formants, min.len = 1, max.len = 5)
  if(!testSubset(specify_formants, c('f1', 'f2', 'f3', 'f4', 'f5')))
    stop(paste0("No formant with the label specified in specify_formants. Accepted labels: f1, f2, f3, f4, f5."))
  
  if (intensity != FALSE & intensity != TRUE)
    stop("intensity MUST be BOOLEAN - TRUE or FALSE")
  
  if (pitch != FALSE & pitch != TRUE)
    stop("pitch MUST be BOOLEAN - TRUE or FALSE")
  
  if (nas_to_zeros != FALSE & nas_to_zeros != TRUE)
    stop("nas_to_zeros MUST be BOOLEAN - TRUE or FALSE")
  
  if (delete_praat_files != FALSE & delete_praat_files != TRUE)
    stop("delete_praat_files MUST be BOOLEAN - TRUE or FALSE")
  
  if (save_as_csv != FALSE & save_as_csv != TRUE)
    stop("save_as_csv MUST be BOOLEAN - TRUE or FALSE")
  
  expect_character(specify_folder, len = 1)
  
  expect_character(specify_name, len = 1)
  
  #------------------------------------------------------------
  #------------------------------------------------------------
  
  mkdirs <- function(fp) {
    if(!file.exists(fp)) {
      mkdirs(dirname(fp))
      dir.create(fp)
    }
  }
  #specify different options for pauses
  name_parameters_df <- as.data.frame(name_parameters)
  user_factors <- rownames(name_parameters_df)
  user_factors_numbers <- name_parameters_df$name_parameters
  
  default_dataframe_names <- c('extraction')
  
  default_dataframe_names <- c(user_factors, default_dataframe_names)
  
  formant_comparisons <- c('f1', 'f2', 'f3', 'f4', 'f5')
  
  #set segment----------------------------------------------------------
  #exists
  if(!is.null(segment_tier)){
    default_dataframe_names <- c(default_dataframe_names, 'segment')
    
    #contiguous
    if(!is.null(contiguous_segment)){
      if(contiguous_segment == 'all'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_segment')
        default_dataframe_names <- c(default_dataframe_names, 'foll_segment')
        default_dataframe_names <- c(default_dataframe_names, 'intervocalic')
      }else if(length(which(contiguous_segment %in% 'prev')) == 1){
        default_dataframe_names <- c(default_dataframe_names, 'prev_segment')
      }else if(length(which(contiguous_segment %in% 'foll')) == 1){
        default_dataframe_names <- c(default_dataframe_names, 'foll_segment')
      }else if(length(which(contiguous_segment %in% 'intvoc')) == 1){
        default_dataframe_names <- c(default_dataframe_names, 'intervocalic')
      }
    }
    
    #set contiguous
    if(!is.null(set_contiguous_segment)){
      if(set_contiguous_segment == 'all'){
        if(!is.null(word_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_same_word')
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_diff_word')
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_same_word')
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_diff_word')
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_same_word')
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_diff_word')
        }
        
        if(!is.null(phrase_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_same_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_diff_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_same_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_diff_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_same_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_diff_phrase')
        }
      }
      
      if(length(which(set_contiguous_segment %in% 'prev')) == 1){
        if(!is.null(word_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_same_word')
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_diff_word')
        }
        
        if(!is.null(phrase_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_same_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'prev_segment_diff_phrase')
        }
      }
      
      if(length(which(set_contiguous_segment %in% 'foll')) == 1){
        if(!is.null(word_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_same_word')
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_diff_word')
        }
        
        if(!is.null(phrase_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_same_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'foll_segment_diff_phrase')
        }
      }
      
      if(length(which(set_contiguous_segment %in% 'intvoc')) == 1){
        if(!is.null(word_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_same_word')
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_diff_word')
        }
        
        if(!is.null(phrase_tier)){
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_same_phrase')
          default_dataframe_names <- c(default_dataframe_names, 'intvoc_diff_phrase')
        }
      }
    }
    
    #position
    if(segment_position){
      
      if(!is.null(word_tier)){
        default_dataframe_names <- c(default_dataframe_names, 'segment_word_position')
      }
      
      if(!is.null(word_tier)){
        default_dataframe_names <- c(default_dataframe_names, 'segment_phrase_position')
      }
    }
  }
  
  #set word----------------------------------------------------------
  #exists
  if(!is.null(word_tier)){
    default_dataframe_names <- c(default_dataframe_names, 'word')
    
    #contiguous
    if(!is.null(contiguous_word)){
      if(contiguous_word == 'all'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_word')
        default_dataframe_names <- c(default_dataframe_names, 'foll_word')
      }else if(contiguous_word == 'prev'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_word')
      }else if(contiguous_word == 'foll'){
        default_dataframe_names <- c(default_dataframe_names, 'foll_word')
      }
    }
    
    #set contiguous
    if(!is.null(set_contiguous_word)){
      if(set_contiguous_word == 'all'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_word_same_phrase')
        default_dataframe_names <- c(default_dataframe_names, 'prev_word_diff_phrase')
        default_dataframe_names <- c(default_dataframe_names, 'foll_word_same_phrase')
        default_dataframe_names <- c(default_dataframe_names, 'foll_word_diff_phrase')
      }else if(set_contiguous_word == 'prev'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_word_same_phrase')
        default_dataframe_names <- c(default_dataframe_names, 'prev_word_diff_phrase')
      }else if(set_contiguous_word == 'foll'){
        default_dataframe_names <- c(default_dataframe_names, 'foll_word_same_phrase')
        default_dataframe_names <- c(default_dataframe_names, 'foll_word_diff_phrase')
      }
    }
    
    #position
    if(word_position){
      default_dataframe_names <- c(default_dataframe_names, 'word_phrase_position')
    }
  }
  
  #set phrase----------------------------------------------------------
  #exists
  if(!is.null(phrase_tier)){
    default_dataframe_names <- c(default_dataframe_names, 'phrase')
    
    #contiguous
    if(!is.null(contiguous_phrase)){
      if(contiguous_phrase == 'all'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_phrase')
        default_dataframe_names <- c(default_dataframe_names, 'foll_phrase')
      }else if(contiguous_phrase == 'prev'){
        default_dataframe_names <- c(default_dataframe_names, 'prev_phrase')
      }else if(contiguous_phrase == 'foll'){
        default_dataframe_names <- c(default_dataframe_names, 'foll_phrase')
      }
    }
    
  }
  #---------------------------------------------------------------------
  
  if(!is.null(pause_marker)){
    default_dataframe_names <- c(default_dataframe_names, 'pre_pausal')
    default_dataframe_names <- c(default_dataframe_names, 'post_pausal')
  }
  
  if(middle){
    default_dataframe_names <- c(default_dataframe_names, 'middle')
  }
  
  if(onset){
    default_dataframe_names <- c(default_dataframe_names, 'onset')
  }
  
  if(offset){
    default_dataframe_names <- c(default_dataframe_names, 'offset')
  }
  
  if(mean_value){
    default_dataframe_names <- c(default_dataframe_names, 'mean')
  }
  
  if(duration){
    default_dataframe_names <- c(default_dataframe_names, 'duration')
  }
  
  if(formants){
    formant_ids = c()
    for(i in 1:length(specify_formants)){
      formant_ids[i] <- which(formant_comparisons %in% specify_formants[i])
      default_dataframe_names <- c(default_dataframe_names, specify_formants[i])
    }
    number_of_formants <- length(formant_ids)
  }
  
  if(intensity){
    default_dataframe_names <- c(default_dataframe_names, 'intensity')
  }
  
  if(pitch){
    default_dataframe_names <- c(default_dataframe_names, 'pitch')
  }
  
  #creates dataframe header
  df = data.frame(matrix(nrow = 0, ncol = length(default_dataframe_names)))
  names(df) <- default_dataframe_names
  
  dirs <- paste0(getwd(), '/', incoming_list$folder_path)
  FullPath <- function(FileName){ return( paste( dirs, FileName, sep="") ) }
  
  #define points
  if(extraction == 'points'){
    if(extraction_param > 1){
      initial_point <- 1
      end_point <- extraction_param
      max_steps <- end_point
    }else{
      initial_point <- 1
      end_point <- 1
      max_steps <- end_point
    }
  }else if(extraction == 'percentage'){
    initial_point <- 1
    end_point <- length(extraction_param)
    max_steps <- end_point
  }else if(extraction == 'every'){
    initial_point <- 1
    end_point <- 10000
    max_steps <- end_point
  }
  
  #convert these to functions
  formant_params = data.frame(
    time_step_sec <- 0.0,
    max_number_of_formants <- 5.0,
    max_formant_hz <- 5500,
    window_length_sec <- 0.025,
    pre_emphasis_from_hz <- 50,
    overwrite <- T,
    unit <- 'hertz',
    interpolation <- 'Linear'
  )
  
  intensity_params = data.frame(
    minimum_pitch_hz <- 100,
    time_step_sec <- 0.0,
    subtract_mean <- 'yes',
    overwrite <- T,
    interpolation <- 'cubic',
    mean_averaging_method <- 'energy'
  )
  
  pitch_params <- data.frame(
    time_step_sec <- 0.0,
    pitch_floor_hz <- 75,
    pitch_ceiling_hz <- 600,
    overwrite <- T,
    unit <- 'hertz',
    interpolation <- 'Linear'
  )
  
  overall_token <- 1
  
  for(i in 1:length(data_summary)){
    tg <- data_summary[[i]]
    
    tmp_name <- gsub('\\..*', "", tg$tg_name)
    tg_name <- tg$tg_name

    au_name <- paste0(tmp_name, '.wav')
    formant_name <- paste0(tmp_name, '.Formant')
    intensity_name <- paste0(tmp_name, '.Intensity')
    pitch_name <- paste0(tmp_name, '.Pitch')
    
    individual_token <- 1
    #if extraction of points is required
    
    if(formants){
      #creates a formant file
      praat('To Formant (burg)...', list(formant_params$time_step_sec,
                                         formant_params$max_number_of_formants,
                                         formant_params$max_formant_hz,
                                         formant_params$window_length_sec,
                                         formant_params$pre_emphasis_from_hz),
            input=FullPath(au_name), output=FullPath(formant_name), overwrite=formant_params$overwrite)
    }
    
    if(intensity){
      praat('To Intensity...', list(intensity_params$minimum_pitch_hz,
                                    intensity_params$time_step_sec,
                                    intensity_params$subtract_mean),
            input=FullPath(au_name), output=FullPath(intensity_name), overwrite=intensity_params$overwrite)
    }
    
    if(pitch){
      praat('To Pitch...', list(pitch_params$time_step_sec,
                                pitch_params$pitch_floor_hz,
                                pitch_params$pitch_ceiling_hz),
            input=FullPath(au_name), output=FullPath(pitch_name), overwrite=pitch_params$overwrite)
    }
    
    if(!is.null(segment_tier)){
      #Count intervals in text grid
      interval_nmbr_segment_tier <- as.numeric(praat("Get number of intervals...", list(segment_tier),
                                                    input=FullPath(tg_name), simplify=TRUE))
    }
    
    if(!is.null(word_tier)){
      #get number of intervals at the word tier
      interval_nmbr_word_tier <- as.numeric(praat("Get number of intervals...", list(word_tier),
                                                 input=FullPath(tg_name), simplify=TRUE))
    }
    
    if(!is.null(phrase_tier)){
      #get number of intervals at the phrase tier
      interval_nmbr_phrase_tier <- as.numeric(praat("Get number of intervals...", list(phrase_tier),
                                                   input=FullPath(tg_name), simplify=TRUE))
    }
    
    #counts intervals for phrases, words and intervals to later be retrieved for location information
    #--------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------
    #segments-----------------------
    if(!is.null(segment_tier) && segment_position){
      tmp_initial_point_segment <- c()
      tmp_end_point_segment <- c()
      for(position_segment in 1:interval_nmbr_segment_tier){
        tmp_initial_point_segment[position_segment] <- as.numeric(praat("Get start point...", list(segment_tier, position_segment),
                                                                       input=FullPath(tg_name), simplify=TRUE))
        tmp_end_point_segment[position_segment] <- as.numeric(praat("Get end point...", list(segment_tier, position_segment),
                                                                   input=FullPath(tg_name), simplify=TRUE))
      }
      
      segment_counts <- data.frame(tmp_initial_point_segment, tmp_end_point_segment)
      names(segment_counts) <- c('initial', 'end')
      segment_counts <- data.table(segment_counts)
    }
    
    #words--------------------------
    if(!is.null(word_tier) && word_position){
      tmp_initial_point_word <- c()
      tmp_end_point_word <- c()
      for(position_word in 1:interval_nmbr_word_tier){
        tmp_initial_point_word[position_word] <- as.numeric(praat("Get start point...", list(word_tier, position_word),
                                                                 input=FullPath(tg_name), simplify=TRUE))
        tmp_end_point_word[position_word] <- as.numeric(praat("Get end point...", list(word_tier, position_word),
                                                             input=FullPath(tg_name), simplify=TRUE))
      }
      
      if(!is.null(segment_tier) && segment_position){
        word_location_segment <- vector(mode="numeric", length=length( tmp_initial_point_segment))
        for(word_i in 1:length(tmp_initial_point_word)){
          
          word_location_segment[which(tmp_initial_point_segment >= tmp_initial_point_word[word_i] &
                                        tmp_initial_point_segment <= tmp_end_point_word[word_i] &
                                        tmp_end_point_segment >= tmp_initial_point_word[word_i] &
                                        tmp_end_point_segment <= tmp_end_point_word[word_i])] <- word_i
          
        }
        segment_counts$words <- word_location_segment
        
        #counts
        segment_counts[ , segment_in_words := 1:.N , by = c("words") ]
        
        tmp_segment_in_words_count <- ddply(segment_counts, .(words), summarise, max_values=max(segment_in_words))
        
        segment_counts$segment_in_words_count <- 0
        for(segment_word_count_i in 1:nrow(tmp_segment_in_words_count)){
          segment_counts[segment_counts$words == tmp_segment_in_words_count$words[segment_word_count_i],c('segment_in_words_count')] <- tmp_segment_in_words_count$max_values[segment_word_count_i]
        }
        
        segment_counts$word_position <- ifelse(segment_counts$segment_in_words == 1, 'initial', 'medial')
        segment_counts[segment_counts$segment_in_words == segment_counts$segment_in_words_count, c('word_position')] <-  'final'
        
      }
    }
    
    #phrases------------------------
    if(!is.null(phrase_tier)){
      tmp_initial_point_phrase <- c()
      tmp_end_point_phrase <- c()
      for(position_phrase in 1:interval_nmbr_phrase_tier){
        tmp_initial_point_phrase[position_phrase] <- as.numeric(praat("Get start point...", list(phrase_tier, position_phrase),
                                                                     input=FullPath(tg_name), simplify=TRUE))
        tmp_end_point_phrase[position_phrase] <- as.numeric(praat("Get end point...", list(phrase_tier, position_phrase),
                                                                 input=FullPath(tg_name), simplify=TRUE))
      }
      
      
      if(!is.null(segment_tier) && segment_position){
        phrase_location_segment <- vector(mode="numeric", length=length( tmp_initial_point_segment))
        for(phrase_i in 1:length(tmp_initial_point_phrase)){
          phrase_location_segment[which(tmp_initial_point_segment >= tmp_initial_point_phrase[phrase_i] &
                                          tmp_initial_point_segment <= tmp_end_point_phrase[phrase_i] &
                                          tmp_end_point_segment >= tmp_initial_point_phrase[phrase_i] &
                                          tmp_end_point_segment <= tmp_end_point_phrase[phrase_i])] <- phrase_i
          
        }
        
        segment_counts$phrases <- phrase_location_segment
        
        #segment counts in phrases
        segment_counts[ , segment_in_phrases := 1:.N , by = c("phrases") ]
        
        tmp_segment_in_phrases_count <- ddply(segment_counts, .(phrases), summarise, max_values=max(segment_in_phrases))
        
        segment_counts$segment_in_phrases_count <- 0
        for(segment_phrase_count_i in 1:nrow(tmp_segment_in_phrases_count)){
          segment_counts[segment_counts$phrases == tmp_segment_in_phrases_count$phrases[segment_phrase_count_i],c('segment_in_phrases_count')] <- tmp_segment_in_phrases_count$max_values[segment_phrase_count_i]
        }
        
        segment_counts$phrase_position <- ifelse(segment_counts$segment_in_phrases == 1, 'initial', 'medial')
        segment_counts[segment_counts$segment_in_phrases == segment_counts$segment_in_phrases_count, c('phrase_position')] <- 'final'
        
      }
      
      #saves word counts in phrases
      
      if(!is.null(word_tier) && word_position){
        phrase_location_word <- vector(mode="numeric", length=length( tmp_initial_point_word))
        for(phrase_i in 1:length(tmp_initial_point_phrase)){
          phrase_location_word[which(tmp_initial_point_word >= tmp_initial_point_phrase[phrase_i] &
                                       tmp_initial_point_word <= tmp_end_point_phrase[phrase_i] &
                                       tmp_end_point_word >= tmp_initial_point_phrase[phrase_i] &
                                       tmp_end_point_word <= tmp_end_point_phrase[phrase_i])] <- phrase_i
          
        }
        
        
        word_counts <- data.frame(phrase_location_word)
        names(word_counts) <- c('phrases')
        word_counts <- data.table(word_counts)
        
        word_counts[ , word_in_phrases := 1:.N , by = c("phrases") ]
        
        tmp_word_in_phrases_count <- ddply(word_counts, .(phrases), summarise, max_values=max(word_in_phrases))
        
        word_counts$word_in_phrases_count <- 0
        for(word_phrase_count_i in 1:nrow(tmp_word_in_phrases_count)){
          word_counts[word_counts$phrases == tmp_word_in_phrases_count$phrases[word_phrase_count_i],c('word_in_phrases_count')] <- tmp_word_in_phrases_count$max_values[word_phrase_count_i]
        }
        word_counts$phrase_position <- ifelse(word_counts$word_in_phrases == 1, 'initial', 'medial')
        word_counts[word_counts$word_in_phrases == word_counts$word_in_phrases_count, c('phrase_position')] <- 'final'
      }
    }
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------------------------------
    
    #Analysis for each tier
    for(interval in 1:interval_nmbr_segment_tier)
    {
      
      #gets the label of the interval
      tmp_intrvl_label <- praat("Get label of interval...",  list(segment_tier, interval),
                               input=FullPath(tg_name))
      
      #checks whether the label is not empty and matches the segment list requested
      if(length(which('all' %in% specify_segments)) == 0 && length(which(tmp_intrvl_label %in% specify_segments)) != 0){
        if(is.null(pause_marker) || (!is.null(pause_marker) && tmp_intrvl_label != pause_marker)){
          
          #get start point
          start_pnt <- as.numeric(praat("Get start point...", list(segment_tier, interval),
                                       input=FullPath(tg_name), simplify=TRUE))
          #get end point
          end_pnt <- as.numeric(praat("Get end point...", list(segment_tier, interval),
                                     input=FullPath(tg_name), simplify=TRUE))
          
          seg_dur <- end_pnt - start_pnt
          
          segment_nmbr <- interval
          
          #Sets the middle point
          mddl_pnt <- start_pnt + ((end_pnt - start_pnt) / 2)
          
          #in the where each point in the vowle is to be extracted every n ms,
          #it checks whether the vowel duration is longer than the threshold
          if((length(which('every' %in% extraction)) == 1 && seg_dur > (extraction_param/1000)) ||
             (length(which('every' %in% extraction)) == 0)){
            
            #segments contiguous tiers---------------------------------------------------------------------------
            
            #segment previous context
            if(!is.null(contiguous_segment)){
              if(contiguous_segment == 'all' || length(which(contiguous_segment %in% 'prev')) == 1){
                if (interval == 1)
                {
                  if(!is.null(pause_marker)){
                    previous_segment <- pause_marker
                  }else{
                    previous_segment <- ""
                  }
                  
                }else{
                  
                  previous_segment <- praat("Get label of interval...",  list(segment_tier, interval-1),
                                           input=FullPath(tg_name))
                  
                }
              }
            }
            
            #segment following context
            if(!is.null(contiguous_segment)){
              if(contiguous_segment == 'all' || length(which(contiguous_segment %in% 'foll')) == 1){
                if (interval == interval_nmbr_segment_tier)
                {
                  
                  if(!is.null(pause_marker)){
                    following_segment <- pause_marker
                  }else{
                    following_segment <- ""
                  }
                  
                }else{
                  
                  following_segment <- praat("Get label of interval...",  list(segment_tier, interval+1),
                                            input=FullPath(tg_name))
                  
                }
              }
            }
            
            #intervocalic context
            if(!is.null(contiguous_segment)){
              if(contiguous_segment == 'all' || length(which(contiguous_segment %in% 'intvoc')) == 1){
                
                if (interval == 1)
                {
                  if(!is.null(pause_marker)){
                    previous_segment <- pause_marker
                  }else{
                    previous_segment <- ""
                  }
                  
                }else{
                  
                  previous_segment <- praat("Get label of interval...",  list(segment_tier, interval-1),
                                           input=FullPath(tg_name))
                  
                }
                
                if (interval == interval_nmbr_segment_tier)
                {
                  if(!is.null(pause_marker)){
                    following_segment <- pause_marker
                  }else{
                    following_segment <- ""
                  }
                  
                }else{
                  
                  following_segment <- praat("Get label of interval...",  list(segment_tier, interval+1),
                                            input=FullPath(tg_name))
                  
                }
                
                
                if (length(which(previous_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0 &
                    length(which(following_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0)
                {
                  is_intervocalic <- 'y'
                }else{
                  is_intervocalic <- 'n'
                }
              }
            }
            
            #word tiers---------------------------------------------------------------------------
            if(!is.null(word_tier)){
              #get number of interval at tier 1
              word_nmbr <- as.numeric(praat("Get interval at time...",  list(word_tier, mddl_pnt),
                                           input=FullPath(tg_name), simplify = T))
              
              #get label of the interval number in tier 1
              
              word_token <- praat("Get label of interval...",  list(word_tier, word_nmbr),
                                 input=FullPath(tg_name))
              
              
              #get start point
              start_pnt_word <- as.numeric(praat("Get start point...", list(word_tier, word_nmbr),
                                                input=FullPath(tg_name), simplify=TRUE))
              #get end point
              end_pnt_word <- as.numeric(praat("Get end point...", list(word_tier, word_nmbr),
                                              input=FullPath(tg_name), simplify=TRUE))
              
              word_dur <- end_pnt_word - start_pnt_word
              
              #Sets the middle point
              mddl_pnt_word <- start_pnt_word + ((end_pnt_word - start_pnt_word) / 2)
            }
            
            #phrase tiers---------------------------------------------------------------------------
            if(!is.null(phrase_tier)){
              #get number of interval at tier 1
              phrase_nmbr <- as.numeric(praat("Get interval at time...",  list(phrase_tier, mddl_pnt),
                                             input=FullPath(tg_name), simplify=TRUE))
              
              #get label of the interval number in tier 1
              
              phrase_token <- praat("Get label of interval...",  list(phrase_tier, phrase_nmbr),
                                   input=FullPath(tg_name))
              
              
              #get start point
              start_pnt_phrase <- as.numeric(praat("Get start point...", list(phrase_tier, phrase_nmbr),
                                                  input=FullPath(tg_name), simplify=TRUE))
              #get end point
              end_pnt_phrase <- as.numeric(praat("Get end point...", list(phrase_tier, phrase_nmbr),
                                                input=FullPath(tg_name), simplify=TRUE))
              
              phrase_dur <- end_pnt_phrase - start_pnt_phrase
              
              #Sets the middle point
              mddl_pnt_phrase <- start_pnt_phrase + ((end_pnt_phrase - start_pnt_phrase) / 2)
            }
            
            
            
            if(   is.null(phrase_tier)  || (!is.null(phrase_tier) && (is.null(focus_phrases) && is.null(omit_phrases)))   ||
                  (!is.null(phrase_tier) && !is.null(focus_phrases) && length(which(phrase_token %in% focus_phrases)) > 0) ||
                  (!is.null(phrase_tier) && !is.null(omit_phrases) && length(which(phrase_token %in% omit_phrases)) == 0)     )
              
            {
              if(   is.null(word_tier)  || (!is.null(word_tier) && (is.null(focus_words) && is.null(omit_words)))   ||
                    (!is.null(word_tier) && !is.null(focus_words) && length(which(word_token %in% focus_words)) > 0) ||
                    (!is.null(word_tier) && !is.null(omit_words) && length(which(word_token %in% omit_words)) == 0)     )
                
              {
                #contiguous phrases-----------------------------------------------------------------------------
                #---------------------------------------------------------------------------------------------
                #---------------------------------------------------------------------------------------------
                if(!is.null(phrase_tier) & !is.null(contiguous_phrase)){
                  #phrase previous context
                  if(contiguous_phrase == 'all' || contiguous_phrase == 'prev'){
                    if (phrase_nmbr == 1)
                    {
                      previous_phrase <- ""
                    }else{
                      
                      previous_phrase <- praat("Get label of interval...",  list(phrase_tier, phrase_nmbr-1),
                                              input=FullPath(tg_name))
                      
                    }
                  }
                  
                  #phrase following context
                  if(contiguous_phrase == 'all' || contiguous_phrase == 'foll'){
                    if (phrase_nmbr == interval_nmbr_phrase_tier)
                    {
                      following_phrase <- ""
                    }else{
                      following_phrase <- praat("Get label of interval...",  list(phrase_tier, phrase_nmbr+1),
                                               input=FullPath(tg_name))
                    }
                  }
                }
                
                #contiguous words-----------------------------------------------------------------------------
                #---------------------------------------------------------------------------------------------
                #---------------------------------------------------------------------------------------------
                if(!is.null(word_tier) & !is.null(contiguous_word)){
                  #word previous context
                  if(contiguous_word == 'all' || contiguous_word == 'prev'){
                    if (word_nmbr == 1)
                    {
                      previous_word <- ""
                    }else{
                      previous_word <- praat("Get label of interval...",  list(word_tier, word_nmbr-1),
                                            input=FullPath(tg_name))
                    }
                  }
                  
                  #word following context
                  if(contiguous_word == 'all' || contiguous_word == 'foll'){
                    if (word_nmbr == interval_nmbr_word_tier)
                    {
                      following_word <- ""
                    }else{
                      following_word <- praat("Get label of interval...",  list(word_tier, word_nmbr+1),
                                             input=FullPath(tg_name))
                    }
                  }
                }
                
                #set contiguous words
                if(!is.null(phrase_tier) && !is.null(word_tier) & !is.null(set_contiguous_word)){
                  if(set_contiguous_word == 'all' || set_contiguous_word == 'prev'){
                    if (phrase_nmbr == 1)
                    {
                      previous_word_same_phrase <- ""
                      previous_word_diff_phrase <- ""
                    }else{
                      start_pnt_previous_word <- as.numeric(praat("Get start point...", list(word_tier, word_nmbr-1),
                                                                 input=FullPath(tg_name), simplify=TRUE))
                      tmp_previous_word <- praat("Get label of interval...",  list(word_tier, word_nmbr-1),
                                                input=FullPath(tg_name))
                      
                      if(start_pnt_previous_word >= start_pnt_phrase){
                        previous_word_same_phrase <- tmp_previous_word
                        previous_word_diff_phrase <- ""
                      }else{
                        previous_word_same_phrase <- ""
                        previous_word_diff_phrase <- tmp_previous_word
                      }
                    }
                  }
                  
                  
                  if(set_contiguous_word == 'all' || set_contiguous_word == 'foll'){
                    if (phrase_nmbr == interval_nmbr_phrase_tier)
                    {
                      following_word_same_phrase <- ""
                      following_word_diff_phrase <- ""
                    }else{
                      end_pnt_following_word <- as.numeric(praat("Get end point...", list(word_tier, word_nmbr+1),
                                                                input=FullPath(tg_name), simplify=TRUE))
                      tmp_following_word <- praat("Get label of interval...",  list(word_tier, word_nmbr+1),
                                                 input=FullPath(tg_name))
                      
                      if(end_pnt_following_word <= end_pnt_phrase){
                        following_word_same_phrase <- tmp_following_word
                        following_word_diff_phrase <- ""
                      }else{
                        following_word_same_phrase <- ""
                        following_word_diff_phrase <- tmp_following_word
                      }
                    }
                  }
                }
                
                #contiguous segments-----------------------------------------------------------------------------
                #---------------------------------------------------------------------------------------------
                #---------------------------------------------------------------------------------------------
                if(!is.null(segment_tier) & !is.null(contiguous_segment)){
                  #segment previous context
                  if(contiguous_segment == 'all' || contiguous_segment == 'prev'){
                    if (segment_nmbr == 1)
                    {
                      previous_segment <- ""
                    }else{
                      previous_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr-1),
                                               input=FullPath(tg_name))
                    }
                  }
                  
                  #segment following context
                  if(contiguous_segment == 'all' || contiguous_segment == 'foll'){
                    if (segment_nmbr == interval_nmbr_segment_tier)
                    {
                      following_segment <- ""
                    }else{
                      following_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr+1),
                                                input=FullPath(tg_name))
                    }
                  }
                  
                  #segment intervocalic context
                  if(contiguous_segment == 'all' || contiguous_segment == 'intvoc'){
                    #segment previous context
                    if (segment_nmbr == 1)
                    {
                      tmp_previous_segment <- ""
                    }else{
                      tmp_previous_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr-1),
                                                   input=FullPath(tg_name))
                    }
                    
                    #segment following context
                    if (segment_nmbr == interval_nmbr_segment_tier)
                    {
                      tmp_following_segment <- ""
                    }else{
                      tmp_following_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr+1),
                                                    input=FullPath(tg_name))
                    }
                    
                    #intervocalic
                    if (length(which(tmp_previous_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0 &&
                        length(which(tmp_following_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0)
                    {
                      intervocalic <- 'y'
                    }else{
                      intervocalic <- 'n'
                    }
                  }
                }
                
                
                
                #----------------------
                #set contiguous segments
                #phrases level
                if(!is.null(phrase_tier) && !is.null(segment_tier) & !is.null(set_contiguous_segment)){
                  if(set_contiguous_segment == 'all' || set_contiguous_segment == 'prev'){
                    if (phrase_nmbr == 1)
                    {
                      previous_segment_same_phrase <- ""
                      previous_segment_diff_phrase <- ""
                    }else{
                      start_pnt_previous_segment <- as.numeric(praat("Get start point...", list(segment_tier, segment_nmbr-1),
                                                                    input=FullPath(tg_name), simplify=TRUE))
                      tmp_previous_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr-1),
                                                   input=FullPath(tg_name))
                      
                      if(start_pnt_previous_segment >= start_pnt_phrase){
                        previous_segment_same_phrase <- tmp_previous_segment
                        previous_segment_diff_phrase <- ""
                      }else{
                        previous_segment_same_phrase <- ""
                        previous_segment_diff_phrase <- tmp_previous_segment
                      }
                    }
                  }
                  
                  if(set_contiguous_segment == 'all' || set_contiguous_segment == 'foll'){
                    if (phrase_nmbr == interval_nmbr_phrase_tier)
                    {
                      following_segment_same_phrase <- ""
                      following_segment_diff_phrase <- ""
                    }else{
                      end_pnt_following_segment <- as.numeric(praat("Get end point...", list(segment_tier, segment_nmbr+1),
                                                                   input=FullPath(tg_name), simplify=TRUE))
                      tmp_following_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr+1),
                                                    input=FullPath(tg_name))
                      
                      if(end_pnt_following_segment <= end_pnt_phrase){
                        following_segment_same_phrase <- tmp_following_segment
                        following_segment_diff_phrase <- ""
                      }else{
                        following_segment_same_phrase <- ""
                        following_segment_diff_phrase <- tmp_following_segment
                      }
                    }
                  }
                  
                  if(set_contiguous_segment == 'all' || set_contiguous_segment == 'intvoc'){
                    if (phrase_nmbr == 1)
                    {
                      tmp_previous_segment_same_phrase <- ""
                      tmp_previous_segment_diff_phrase <- ""
                    }else{
                      start_pnt_previous_segment <- as.numeric(praat("Get start point...", list(segment_tier, segment_nmbr-1),
                                                                    input=FullPath(tg_name), simplify=TRUE))
                      tmp_previous_segment = praat("Get label of interval...",  list(segment_tier, segment_nmbr-1),
                                                   input=FullPath(tg_name))
                      
                      if(start_pnt_previous_segment >= start_pnt_phrase){
                        tmp_previous_segment_same_phrase <- tmp_previous_segment
                        tmp_previous_segment_diff_phrase <- ""
                      }else{
                        tmp_previous_segment_same_phrase <- ""
                        tmp_previous_segment_diff_phrase <- tmp_previous_segment
                      }
                    }
                    
                    if (phrase_nmbr == interval_nmbr_phrase_tier)
                    {
                      tmp_following_segment_same_phrase <- ""
                      tmp_following_segment_diff_phrase <- ""
                    }else{
                      end_pnt_following_segment <- as.numeric(praat("Get end point...", list(segment_tier, segment_nmbr+1),
                                                                   input=FullPath(tg_name), simplify=TRUE))
                      tmp_following_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr+1),
                                                    input=FullPath(tg_name))
                      
                      if(end_pnt_following_segment <= end_pnt_phrase){
                        tmp_following_segment_same_phrase <- tmp_following_segment
                        tmp_following_segment_diff_phrase <- ""
                      }else{
                        tmp_following_segment_same_phrase <- ""
                        tmp_following_segment_diff_phrase <- tmp_following_segment
                      }
                    }
                    
                    if (length(which(tmp_previous_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0 &
                        length(which(tmp_following_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0)
                    {
                      #same phrase
                      if(length(which(tmp_previous_segment_same_phrase %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0 &&
                         length(which(tmp_following_segment_same_phrase %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0){
                        intervocalic_same_phrase <- 'y'
                        intervocalic_diff_phrase <- 'n'
                      }else{
                        intervocalic_same_phrase <- 'n'
                        intervocalic_diff_phrase <- 'y'
                      }
                    }else{
                      intervocalic_same_phrase <- 'n'
                      intervocalic_diff_phrase <- 'n'
                    }
                  }
                }
                #--------------
                #----------------------
                
                
                #set contiguous segments
                #words level
                if(!is.null(word_tier) && !is.null(segment_tier) & !is.null(set_contiguous_segment)){
                  if(set_contiguous_segment == 'all' || set_contiguous_segment == 'prev'){
                    if (word_nmbr == 1)
                    {
                      previous_segment_same_word <- ""
                      previous_segment_diff_word <- ""
                    }else{
                      start_pnt_previous_segment <- as.numeric(praat("Get start point...", list(segment_tier, segment_nmbr-1),
                                                                    input=FullPath(tg_name), simplify=TRUE))
                      tmp_previous_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr-1),
                                                   input=FullPath(tg_name))
                      
                      if(start_pnt_previous_segment >= start_pnt_word){
                        previous_segment_same_word <- tmp_previous_segment
                        previous_segment_diff_word <- ""
                      }else{
                        previous_segment_same_word <- ""
                        previous_segment_diff_word <- tmp_previous_segment
                      }
                    }
                  }
                  
                  if(set_contiguous_segment == 'all' || set_contiguous_segment == 'foll'){
                    if (word_nmbr == interval_nmbr_word_tier)
                    {
                      following_segment_same_word <- ""
                      following_segment_diff_word <- ""
                    }else{
                      end_pnt_following_segment <- as.numeric(praat("Get end point...", list(segment_tier, segment_nmbr+1),
                                                                   input=FullPath(tg_name), simplify=TRUE))
                      tmp_following_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr+1),
                                                    input=FullPath(tg_name))
                      
                      if(end_pnt_following_segment <= end_pnt_word){
                        following_segment_same_word <- tmp_following_segment
                        following_segment_diff_word <- ""
                      }else{
                        following_segment_same_word <- ""
                        following_segment_diff_word <- tmp_following_segment
                      }
                    }
                  }
                  
                  if(set_contiguous_segment == 'all' || set_contiguous_segment == 'intvoc'){
                    if (word_nmbr == 1)
                    {
                      tmp_previous_segment_same_word <- ""
                      tmp_previous_segment_diff_word <- ""
                    }else{
                      start_pnt_previous_segment <- as.numeric(praat("Get start point...", list(segment_tier, segment_nmbr-1),
                                                                    input=FullPath(tg_name), simplify=TRUE))
                      tmp_previous_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr-1),
                                                   input=FullPath(tg_name))
                      
                      if(start_pnt_previous_segment >= start_pnt_word){
                        tmp_previous_segment_same_word <- tmp_previous_segment
                        tmp_previous_segment_diff_word <- ""
                      }else{
                        tmp_previous_segment_same_word <- ""
                        tmp_previous_segment_diff_word <- tmp_previous_segment
                      }
                    }
                    
                    if (word_nmbr == interval_nmbr_word_tier)
                    {
                      tmp_following_segment_same_word <- ""
                      tmp_following_segment_diff_word <- ""
                    }else{
                      end_pnt_following_segment <- as.numeric(praat("Get end point...", list(segment_tier, segment_nmbr+1),
                                                                   input=FullPath(tg_name), simplify=TRUE))
                      tmp_following_segment <- praat("Get label of interval...",  list(segment_tier, segment_nmbr+1),
                                                    input=FullPath(tg_name))
                      
                      if(end_pnt_following_segment <= end_pnt_word){
                        tmp_following_segment_same_word <- tmp_following_segment
                        tmp_following_segment_diff_word <- ""
                      }else{
                        tmp_following_segment_same_word <- ""
                        tmp_following_segment_diff_word <- tmp_following_segment
                      }
                    }
                    
                    if (length(which(tmp_previous_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0 &
                        length(which(tmp_following_segment %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0)
                    {
                      #same word
                      if(length(which(tmp_previous_segment_same_word %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0 &&
                         length(which(tmp_following_segment_same_word %in% ipacorr(symbol_format, vowel_set = 'all')$vowels)) != 0){
                        intervocalic_same_word <- 'y'
                        intervocalic_diff_word <- 'n'
                      }else{
                        intervocalic_same_word <- 'n'
                        intervocalic_diff_word <- 'y'
                      }
                    }else{
                      intervocalic_same_word <- 'n'
                      intervocalic_diff_word <- 'n'
                    }
                  }
                }
                
                #================================================================================================
                
                if(extraction == 'points'){
                  #Sets the interval step
                  interval_step <- (end_pnt - start_pnt) / max_steps
                }else if(extraction == 'every'){
                  #Sets the interval step
                  interval_step <- extraction_param/1000
                }
                
                #initial time---------------------------------------
                tmp_tm = start_pnt
                if(formants == T){
                  formant_list <- list()
                }
                if(intensity == T){
                  intensity_vector <- c()
                }
                if(pitch == T){
                  pitch_vector <- c()
                }
                
                if(formants == T){
                  if(middle == T){
                    formant_middle <- c()
                  }
                  
                  if(onset == T){
                    formant_onset <- c()
                  }
                  
                  if(offset == T){
                    formant_offset <- c()
                  }
                  
                  if(mean_value == T){
                    formant_mean <- c()
                  }
                }
                
                j <- 1
                
                while (tmp_tm <= end_pnt){
                  
                  if(extraction == 'percentage'){
                    tmp_tm <- start_pnt + ((extraction_param[j] * seg_dur) / 100)
                  }
                  
                  #intensity--------------------------------------------------------------------------------------------------------
                  if(intensity == T){
                    intensity_vector[j] <- as.numeric(praat("Get value at time...",
                                                           arguments = list(tmp_tm, as.character(intensity_params$interpolation)),
                                                           input=FullPath(intensity_name), simplify=TRUE) )
                    
                    if(middle == T & j == 1){
                      intensity_middle <- as.numeric(praat("Get value at time...",
                                                          arguments = list(mddl_pnt, as.character(intensity_params$interpolation)),
                                                          input=FullPath(intensity_name), simplify=TRUE) )
                    }
                    
                    if(onset == T & j == 1){
                      intensity_onset <- as.numeric(praat("Get value at time...",
                                                         arguments = list(start_pnt, as.character(intensity_params$interpolation)),
                                                         input=FullPath(intensity_name), simplify=TRUE) )
                    }
                    
                    if(offset == T & j == 1){
                      intensity_offset <- as.numeric(praat("Get value at time...",
                                                          arguments = list(end_pnt, as.character(intensity_params$interpolation)),
                                                          input=FullPath(intensity_name), simplify=TRUE) )
                    }
                    
                    if(mean_value == T & j == 1){
                      intensity_mean <- as.numeric(praat("Get mean...",
                                                        arguments = list(start_pnt, end_pnt, as.character(intensity_params$mean_averaging_method)),
                                                        input=FullPath(intensity_name), simplify=TRUE) )
                    }
                    
                  }
                  #pitch--------------------------------------------------------------------------------------------------------
                  if(pitch == T){
                    tmp_pitch_vector <- suppressWarnings(as.numeric(praat("Get value at time...",
                                                                         arguments = list(tmp_tm, as.character(pitch_params$unit),
                                                                                          as.character(pitch_params$interpolation)),
                                                                         input=FullPath(pitch_name), simplify=TRUE) ))
                    
                    if(nas_to_zeros == T){
                      if(is.na(tmp_pitch_vector)){
                        pitch_vector[j] <- 0
                      }else{
                        pitch_vector[j] <- tmp_pitch_vector
                      }
                    }else{
                      pitch_vector[j] <- tmp_pitch_vector
                    }
                    
                    
                    if(middle == T & j == 1){
                      
                      tmp_pitch_middle <- suppressWarnings(as.numeric(praat("Get value at time...",
                                                                           arguments = list(mddl_pnt, as.character(pitch_params$unit),
                                                                                            as.character(pitch_params$interpolation)),
                                                                           input=FullPath(pitch_name), simplify=TRUE) ))
                      if(nas_to_zeros == T){
                        if(is.na(tmp_pitch_middle)){
                          pitch_middle <- 0
                        }else{
                          pitch_middle <- tmp_pitch_middle
                        }
                      }else{
                        pitch_middle <- tmp_pitch_middle
                      }
                      
                    }
                    
                    if(onset == T & j == 1){
                      
                      tmp_pitch_onset <- suppressWarnings(as.numeric(praat("Get value at time...",
                                                                          arguments = list(start_pnt, as.character(pitch_params$unit),
                                                                                           as.character(pitch_params$interpolation)),
                                                                          input=FullPath(pitch_name), simplify=TRUE) ))
                      if(nas_to_zeros == T){
                        if(is.na(tmp_pitch_onset)){
                          pitch_onset <- 0
                        }else{
                          pitch_onset <- tmp_pitch_onset
                        }
                      }else{
                        pitch_onset <- tmp_pitch_onset
                      }
                      
                    }
                    
                    if(offset == T & j == 1){
                      
                      tmp_pitch_offset <- suppressWarnings(as.numeric(praat("Get value at time...",
                                                                           arguments = list(end_pnt, as.character(pitch_params$unit),
                                                                                            as.character(pitch_params$interpolation)),
                                                                           input=FullPath(pitch_name), simplify=TRUE) ))
                      if(nas_to_zeros == T){
                        if(is.na(tmp_pitch_offset)){
                          pitch_offset <- 0
                        }else{
                          pitch_offset <- tmp_pitch_offset
                        }
                      }else{
                        pitch_offset <- tmp_pitch_offset
                      }
                      
                    }
                    
                    if(mean_value == T & j == 1){
                      
                      tmp_pitch_mean <- suppressWarnings(as.numeric(praat("Get mean...",
                                                                         arguments = list(start_pnt, end_pnt, as.character(pitch_params$unit)),
                                                                         input=FullPath(pitch_name), simplify=TRUE) ))
                      if(nas_to_zeros == T){
                        if(is.na(tmp_pitch_mean)){
                          pitch_mean <- 0
                        }else{
                          pitch_mean <- tmp_pitch_mean
                        }
                      }else{
                        pitch_mean <- tmp_pitch_mean
                      }
                      
                    }
                    
                  }
                  #formants--------------------------------------------------------------------------------------------------------
                  if(formants == T){
                    
                    for(k in 1:number_of_formants){
                      if(j == 1){
                        tmp_formant_value <- as.numeric(praat("Get value at time...",
                                                             arguments = list(formant_ids[k], tmp_tm,
                                                                              as.character(formant_params$unit),
                                                                              as.character(formant_params$interpolation)),
                                                             input=FullPath(formant_name), simplify=TRUE) )
                        
                        if(nas_to_zeros == T){
                          if(is.na(tmp_formant_value)){
                            
                            formant_list[[k]] <- 0
                            
                          }else{
                            formant_list[[k]] <- tmp_formant_value
                          }
                        }else{
                          formant_list[[k]] <- tmp_formant_value
                        }
                        
                      }else{
                        tmp_formant_value <- as.numeric(praat("Get value at time...",
                                                             arguments = list(formant_ids[k], tmp_tm,
                                                                              as.character(formant_params$unit),
                                                                              as.character(formant_params$interpolation)),
                                                             input=FullPath(formant_name), simplify=TRUE) )
                        
                        if(nas_to_zeros == T){
                          if(is.na(tmp_formant_value)){
                            formant_list[[k]] <- append(formant_list[[k]], 0)
                          }else{
                            formant_list[[k]] <- append(formant_list[[k]], tmp_formant_value)
                          }
                        }else{
                          formant_list[[k]] <- append(formant_list[[k]], tmp_formant_value)
                        }
                      }
                      
                      if(middle == T & j == 1){
                        tmp_formant_value <- as.numeric(praat("Get value at time...",
                                                             arguments = list(formant_ids[k], mddl_pnt,
                                                                              as.character(formant_params$unit),
                                                                              as.character(formant_params$interpolation)),
                                                             input=FullPath(formant_name), simplify=TRUE) )
                        
                        if(nas_to_zeros == T){
                          if(is.na(tmp_formant_value)){
                            formant_middle[k] <- 0
                          }else{
                            formant_middle[k] <- tmp_formant_value
                          }
                        }else{
                          formant_middle[k] <- tmp_formant_value
                        }
                      }
                      
                      if(onset == T & j == 1){
                        tmp_formant_value <- as.numeric(praat("Get value at time...",
                                                             arguments = list(formant_ids[k], start_pnt,
                                                                              as.character(formant_params$unit),
                                                                              as.character(formant_params$interpolation)),
                                                             input=FullPath(formant_name), simplify=TRUE) )
                        
                        if(nas_to_zeros == T){
                          if(is.na(tmp_formant_value)){
                            formant_onset[k] <- 0
                          }else{
                            formant_onset[k] <- tmp_formant_value
                          }
                        }else{
                          formant_onset[k] <- tmp_formant_value
                        }
                      }
                      
                      if(offset == T & j == 1){
                        tmp_formant_value <- as.numeric(praat("Get value at time...",
                                                             arguments = list(formant_ids[k], end_pnt,
                                                                              as.character(formant_params$unit),
                                                                              as.character(formant_params$interpolation)),
                                                             input=FullPath(formant_name), simplify=TRUE) )
                        
                        if(nas_to_zeros == T){
                          if(is.na(tmp_formant_value)){
                            formant_offset[k] <- 0
                          }else{
                            formant_offset[k] <- tmp_formant_value
                          }
                        }else{
                          formant_offset[k] <- tmp_formant_value
                        }
                      }
                      
                      if(mean_value == T & j == 1){
                        tmp_formant_value <- as.numeric(praat("Get mean...",
                                                             arguments = list(formant_ids[k], start_pnt, end_pnt,
                                                                              as.character(formant_params$unit)),
                                                             input=FullPath(formant_name), simplify=TRUE) )
                        
                        if(nas_to_zeros == T){
                          if(is.na(tmp_formant_value)){
                            formant_mean[k] <- 0
                          }else{
                            formant_mean[k] <- tmp_formant_value
                          }
                        }else{
                          formant_mean[k] <- tmp_formant_value
                        }
                      }
                      
                    }
                  }
                  
                  if(extraction != 'every'){
                    
                    if(j < end_point){
                      j <- j + 1
                      
                      if(extraction == 'points'){
                        tmp_tm <- tmp_tm + interval_step
                      }
                    }else{
                      tmp_tm <- end_pnt + 1
                    }
                    
                  }else{
                    max_steps <- j
                    tmp_tm <- tmp_tm + interval_step
                    j <- j + 1
                  }
                }
                
                #Temporal data storing
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #----------------------------------------------------------------------------------------------------------------------------------------------------------
                #stores current loop values in a temporal dataframe
                tmp_df <- data.frame(matrix(nrow = max_steps, ncol = 0))
                
                for(i in 1:length(user_factors_numbers)){
                  tmp_value <- rep(get_name_parameter(tmp_name, name_separator, user_factors_numbers[i]), max_steps)
                  tmp_df[[paste0(user_factors[i])]] <- tmp_value
                }
                
                
                
                #set segment----------------------------------------------------------
                #exists
                if(!is.null(segment_tier)){
                  tmp_df$segment <- tmp_intrvl_label
                  
                  #contiguous
                  if(!is.null(contiguous_segment)){
                    if(contiguous_segment == 'all'){
                      tmp_df$previous_segment <- previous_segment
                      tmp_df$following_segment <- following_segment
                      tmp_df$intervocalic <- is_intervocalic
                    }else if(length(which(contiguous_segment %in% 'prev')) == 1){
                      tmp_df$previous_segment <- previous_segment
                    }else if(length(which(contiguous_segment %in% 'foll')) == 1){
                      tmp_df$following_segment <- following_segment
                    }else if(length(which(contiguous_segment %in% 'intvoc')) == 1){
                      tmp_df$intervocalic <- is_intervocalic
                    }
                  }
                  
                  #set contiguous
                  if(!is.null(set_contiguous_segment)){
                    if(set_contiguous_segment == 'all'){
                      if(!is.null(word_tier)){
                        tmp_df$previous_segment_same_word <- previous_segment_same_word
                        tmp_df$previous_segment_diff_word <- previous_segment_diff_word
                        tmp_df$following_segment_same_word <- following_segment_same_word
                        tmp_df$following_segment_diff_word <- following_segment_diff_word
                        tmp_df$intervocalic_same_word <- intervocalic_same_word
                        tmp_df$intervocalic_diff_word <- intervocalic_diff_word
                      }
                      
                      if(!is.null(phrase_tier)){
                        tmp_df$previous_segment_same_phrase <- previous_segment_same_phrase
                        tmp_df$previous_segment_diff_phrase <- previous_segment_diff_phrase
                        tmp_df$following_segment_same_phrase <- following_segment_same_phrase
                        tmp_df$following_segment_diff_phrase <- following_segment_diff_phrase
                        tmp_df$intervocalic_same_phrase <- intervocalic_same_phrase
                        tmp_df$intervocalic_diff_phrase <- intervocalic_diff_phrase
                      }
                    }
                    
                    if(length(which(set_contiguous_segment %in% 'prev')) == 1){
                      if(!is.null(word_tier)){
                        tmp_df$previous_segment_same_word <- previous_segment_same_word
                        tmp_df$previous_segment_diff_word <- previous_segment_diff_word
                      }
                      
                      if(!is.null(phrase_tier)){
                        tmp_df$previous_segment_same_phrase <- previous_segment_same_phrase
                        tmp_df$previous_segment_diff_phrase <- previous_segment_diff_phrase
                      }
                    }
                    
                    if(length(which(set_contiguous_segment %in% 'foll')) == 1){
                      if(!is.null(word_tier)){
                        tmp_df$foll_segment_same_word <- following_segment_same_word
                        tmp_df$foll_segment_diff_word <- following_segment_diff_word
                      }
                      
                      if(!is.null(phrase_tier)){
                        tmp_df$following_segment_same_phrase <- following_segment_same_phrase
                        tmp_df$following_segment_diff_phrase <- following_segment_diff_phrase
                      }
                    }
                    
                    if(length(which(set_contiguous_segment %in% 'intvoc')) == 1){
                      if(!is.null(word_tier)){
                        tmp_df$intervocalic_same_word <- intervocalic_same_word
                        tmp_df$intervocalic_diff_word <- intervocalic_diff_word
                      }
                      
                      if(!is.null(phrase_tier)){
                        tmp_df$intervocalic_same_phrase <- intervocalic_same_phrase
                        tmp_df$intervocalic_diff_phrase <- intervocalic_diff_phrase
                      }
                    }
                  }
                  
                  #position
                  if(segment_position){
                    
                    if(!is.null(word_tier)){
                      tmp_df$segment_word_position <- segment_counts$word_position[interval]
                    }
                    
                    if(!is.null(word_tier)){
                      tmp_df$segment_phrase_position <- segment_counts$phrase_position[interval]
                    }
                  }
                }
                
                #set word----------------------------------------------------------
                #exists
                if(!is.null(word_tier)){
                  tmp_df$word <- word_token
                  
                  #contiguous
                  if(!is.null(contiguous_word)){
                    if(contiguous_word == 'all'){
                      tmp_df$previous_word <- previous_word
                      tmp_df$following_word <- following_word
                    }else if(contiguous_word == 'prev'){
                      tmp_df$previous_word <- previous_word
                    }else if(contiguous_word == 'foll'){
                      tmp_df$following_word <- following_word
                    }
                  }
                  
                  #set contiguous
                  if(!is.null(set_contiguous_word)){
                    if(set_contiguous_word == 'all'){
                      tmp_df$previous_word_same_phrase <- previous_word_same_phrase
                      tmp_df$previous_word_diff_phrase <- previous_word_diff_phrase
                      tmp_df$following_word_same_phrase <- following_word_same_phrase
                      tmp_df$following_word_diff_phrase <- following_word_diff_phrase
                    }else if(set_contiguous_word == 'prev'){
                      tmp_df$previous_word_same_phrase <- previous_word_same_phrase
                      tmp_df$previous_word_diff_phrase <- previous_word_diff_phrase
                    }else if(set_contiguous_word == 'foll'){
                      tmp_df$following_word_same_phrase <- following_word_same_phrase
                      tmp_df$following_word_diff_phrase <- following_word_diff_phrase
                    }
                  }
                  
                  #position
                  if(word_position == T){
                    tmp_df$word_phrase_position <- word_counts$phrase_position[word_nmbr]
                  }
                }
                
                #set phrase----------------------------------------------------------
                #exists
                if(!is.null(phrase_tier)){
                  tmp_df$phrase <- phrase_token
                  
                  #contiguous
                  if(!is.null(contiguous_phrase)){
                    if(contiguous_phrase == 'all'){
                      tmp_df$previous_phrase <- previous_phrase
                      tmp_df$following_phrase <- following_phrase
                    }else if(contiguous_phrase == 'prev'){
                      tmp_df$previous_phrase <- previous_phrase
                    }else if(contiguous_phrase == 'foll'){
                      tmp_df$following_phrase <- following_phrase
                    }
                  }
                }
                #---------------------------------------------------------------------
                
                #duration information
                if(duration == T){
                  tmp_df$segment_duration <- seg_dur
                  
                  if(!is.null(word_tier)){
                    tmp_df$word_duration <- word_dur
                  }
                  
                  if(!is.null(phrase_tier)){
                    tmp_df$phrase_duration <- phrase_dur
                  }
                }
                
                #basic time information
                tmp_df$start_point_segment <- start_pnt
                tmp_df$end_point_segment <- end_pnt
                tmp_df$middle_point_segment <- mddl_pnt
                
                #words information
                if(!is.null(word_tier)){
                  tmp_df$start_point_word <- start_pnt_word
                  tmp_df$end_point_word <- end_pnt_word
                  tmp_df$middle_point_word <- mddl_pnt_word
                }
                #phrases information
                if(!is.null(phrase_tier)){
                  tmp_df$start_point_phrase <- start_pnt_phrase
                  tmp_df$end_point_phrase <- end_pnt_phrase
                  tmp_df$middle_point_phrase <- mddl_pnt_phrase
                }
                
                
                
                
                
                #stores individual token number
                tmp_df$individual_token <- individual_token
                #adds +1 to count the next individual token
                individual_token <- individual_token + 1
                #stores overall token number
                tmp_df$overall_token <- overall_token
                #adds +1 to count the next overall token
                overall_token <- overall_token + 1
                
                if(extraction == 'points'){
                  tmp_df$extraction_param <- 1:max_steps
                }else if(extraction == 'percentage'){
                  tmp_df$extraction_param <- extraction_param
                }else if(extraction == 'every'){
                  tmp_df$extraction_param <- seq(0, ((extraction_param*max_steps)-extraction_param), extraction_param)
                }
                
                
                
                
                
                
                #intensity settings
                if(intensity == T){
                  tmp_df$intensity <- intensity_vector
                  
                  if(middle == T){
                    tmp_df$intensity_middle <- intensity_middle
                  }
                  
                  if(onset == T){
                    tmp_df$intensity_onset <- intensity_onset
                  }
                  
                  if(offset == T){
                    tmp_df$intensity_offset <- intensity_offset
                  }
                  
                  if(mean_value == T){
                    tmp_df$intensity_mean <- intensity_mean
                  }
                }
                
                if(pitch == T){
                  tmp_df$pitch <- pitch_vector
                  
                  if(middle == T){
                    tmp_df$pitch_middle <- pitch_middle
                  }
                  
                  if(onset == T){
                    tmp_df$pitch_onset <- pitch_onset
                  }
                  
                  if(offset == T){
                    tmp_df$pitch_offset <- pitch_offset
                  }
                  
                  if(mean_value == T){
                    tmp_df$pitch_mean <- pitch_mean
                  }
                }
                
                if(formants == T){
                  for(k in 1:number_of_formants){
                    tmp_df[[specify_formants[k]]] <- formant_list[[k]]
                  }
                  
                  if(middle == T){
                    for(k in 1:number_of_formants){
                      tmp_df[[paste0('middle_', specify_formants[k])]] <- formant_middle[k]
                    }
                  }
                  
                  if(onset == T){
                    for(k in 1:number_of_formants){
                      tmp_df[[paste0('onset_', specify_formants[k])]] <- formant_onset[k]
                    }
                  }
                  
                  if(offset == T){
                    for(k in 1:number_of_formants){
                      tmp_df[[paste0('offset_', specify_formants[k])]] <- formant_offset[k]
                    }
                  }
                  
                  if(mean_value == T){
                    for(k in 1:number_of_formants){
                      tmp_df[[paste0('mean_', specify_formants[k])]] <- formant_mean[k]
                    }
                  }
                  
                }
                
                #saves to the main data frame
                df <- rbind(df, tmp_df)
                write.csv(df, 'tmpdf.csv')
              }
            }
          }
        }
      }
    }
    
    #deletes praat files
    if(delete_praat_files == T){
      if(formants == T){
        unlink(paste0(incoming_list$folder_path, formant_name))
      }
      if(intensity  == T){
        unlink(paste0(incoming_list$folder_path, intensity_name))
      }
      if(pitch == T){
        unlink(paste0(incoming_list$folder_path, pitch_name))
      }
    }
  }
  
  if(save_as_csv == T){
    if(!is.null(specify_folder)){
      mkdirs(specify_folder)
      
      #name
      if(is.null(specify_name)){
        file_name <- 'df.csv'
      }else{
        if(specify_name == 'asdate'){
          file_name <- gsub(':', '_', date())
          file_name <- gsub(' ', '_', file_name)
          file_name <- paste0(file_name, '.csv')
        }else{
          file_name <- paste0(specify_name, '.csv')
        }
      }
      
      #save file
      write.csv(df, paste0(specify_folder, '/', file_name))
    }
    
  }
  
  return(df)
}
