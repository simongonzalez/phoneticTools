\name{filter_word_syllables}
\alias{filter_word_syllables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Word syllable number filtering
%%  ~~function to do ... ~~
}
\description{
Filters words based on number of syllables
}
\usage{
filter_word_syllables(data_set = NULL,
                      column_header = NULL,
                      number_of_syllables = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the words to be filtered
}
  \item{column_header}{
character vector containing the header of the column containing the words to be filtered
  }
  \item{number_of_syllables}{
number of syllables to be filtered
  }


}
\details{
Output:

Exports the same dataset with the same number of columns containing the filtered elements
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
new_filtering = filter_word_syllables(data_set = input_data,
                                      column_header = 'word',
                                      number_of_syllables = c(2,3))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
