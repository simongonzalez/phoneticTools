\name{get_symbols}
\alias{get_symbols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gets Phonetic Symbols
%%  ~~function to do ... ~~
}
\description{
Gets phonetic symbols
}
\usage{
get_symbols(symbol_format = 'ipa', consonant_set = 'all', vowel_set = 'all',
                   consonant_set_manual = NULL, vowel_set_manual = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol_format}{
transcription type
}
  \item{consonant_set}{
    consonant subset specification (based on features)
}
  \item{vowel_set}{
    vowel subset specification (based on features)
}
  \item{consonant_set_manual}{
    manual list of consonants
}
  \item{vowel_set_manual}{
  manual list of vowels
}


}
\details{
Output:

Vector list containing the segments specified
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
MAUS_segments = get_symbols('maus')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
