\name{get_values}
\alias{get_values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gets acoustic values
%%  ~~function to do ... ~~
}
\description{
Gets acoustic values from wav files and their corresponding TextGrids
}
\usage{
get_values(incoming_list = NULL, data_summary = NULL,
           name_separator = '_', name_parameters = NULL,
           segment_tier = NULL, word_tier = NULL, phrase_tier = NULL,
           specify_segments = NULL, add_segment = NULL, delete_segment = NULL,
           focus_words = NULL, omit_words = NULL, focus_phrases = NULL, omit_phrases = NULL,
           contiguous_segment = 'all', contiguous_word = 'all', contiguous_phrase = 'all',
           segment_position = T, word_position = T, pauses = T,
           set_contiguous_segment = 'all',  set_contiguous_word = 'all',
           pause_marker = '<p:>', extraction = 'points', extraction_param = 5, symbol_set = 'maus',
           onset = T, middle = T, offset = T, mean_value = T,
           duration = T, formants = T, specify_formants = NULL, intensity = T, pitch = T, nas_to_zeros = T,
           delete_praat_files = T, save_as_csv = T, specify_folder = NULL, specify_name = 'asdate')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incoming_list}{
List containing the location and names of files
  }
  \item{data_summary}{
  List containing the summary of the TextGrids
  }
  \item{name_separator}{
    separator to obtain parameters from file names
  }

  \item{name_parameters}{
    location of parameters in file names
  }
  \item{segment_tier}{
   Number of segment tier in the TextGrid
  }
  \item{word_tier}{
    Number of word tier in the TextGrid
  }


  \item{phrase_tier}{
    Number of phrase tier in the TextGrid
  }
  \item{specify_segments}{
   manual segment list
  }
  \item{add_segment}{
    segments to be added to the default list
  }

  \item{delete_segment}{
    segments to be omitted in the default list
  }
  \item{focus_words}{
   manual word list
  }
  \item{omit_words}{
    words to be omitted in the default list
  }

  \item{focus_phrases}{
    manual phrase list
  }
  \item{omit_phrases}{
   phrases to be omitted in the default list
  }
  \item{contiguous_segment}{
    coding contiguous segments
  }



  \item{contiguous_word}{
   coding contiguous words
  }
  \item{contiguous_phrase}{
  coding phrases
  }
  \item{segment_position}{
    coding segment position in the word(initial, medial, final)
  }




  \item{word_position}{
  coding segment position in the phrase(initial, medial, final)
  }
  \item{pauses}{
  code pauses
  }
  \item{set_contiguous_segment}{
    contiguous segment based on word
  }




  \item{set_contiguous_word}{
  contiguous segment based on phrase
  }
  \item{pause_marker}{
  specify pause marker
  }
  \item{extraction}{
    type of data extraction
  }




  \item{extraction_param}{
  number of points in the data extraction
  }
  \item{symbol_set}{
  symbol set in whcih the data is coded
  }
  \item{onset}{
   code values in the onset position of the vowel
  }



  \item{middle}{
 code values in the middle position of the vowel
  }
  \item{offset}{
  code values in the offset position of the vowel
  }
  \item{mean_value}{
   code mean values of vowels
  }




  \item{duration}{
code vowel duration
  }
  \item{formants}{
  code formant values of vowels
  }
  \item{specify_formants}{
  specify which formants are to be extracted
  }




  \item{intensity}{
code for intensity
  }
  \item{pitch}{
code for pitch
  }
  \item{nas_to_zeros}{
  convert NA values to zeros
  }




  \item{delete_praat_files}{
delete praat files after coding
  }
  \item{save_as_csv}{
save dataframe into csv
  }
  \item{specify_folder}{
specifies the folder where the dataframe is saved
  }
  \item{specify_name}{
specify the name of the file, default 'as date'
  }




}
\details{
Output:

data.frame containing all the values specified
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Simon Gonzalez - simon.gonzalez@anu.edu.au
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
new_df = get_values(incoming_list = fl_lst, data_summary = fl_sum,
           name_parameters = c('speaker' = 2, 'gender' = 3, 'school' = 5, 'speechStyle' = 6),
           segment_tier = 3, word_tier = 2, phrase_tier = 1,
           specify_segments = seg_lst$vowels,
           specify_formants = c('f1', 'f2'), specify_folder = 'incomingdata')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
