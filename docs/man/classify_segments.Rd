\name{classify_segments}
\alias{classify_segments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Classify Segments
%%  ~~function to do ... ~~
}
\description{
Classifies segments into phonological and phonetic features
}
\usage{

classify_segments(data_set = NULL, column_header = NULL,
                  criteria = NULL, transcription_type = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the segments to be classifed
  }
  \item{column_header}{
character vector containing the header of the column containing the segments to be classified
  }
  \item{criteria}{
character vector containing the phonetic/phonological features to classify segments
  }
  \item{transcription_type}{
character variable containing the name of the phonetic transcription convention used
}


}
\details{
Output:

  Exports the same dataset with new columns containing the phonetic/phonological classification of segments
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
new_classification = classify_segments(data_set = input_data,
                     column_header = 'previous_segment',
                     criteria = c('phonological_place', 'phonetic_place', 'phonetic_manner'),
                     transcription_type = 'maus')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
