\name{filter_strings}
\alias{filter_strings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Strings Filtering
%%  ~~function to do ... ~~
}
\description{
Filters strings based on user input parameters
}
\usage{
filter_strings(data_set = NULL, column_header = NULL,
               orthography_type = NULL,
               orthography_filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the strings to be filtered
}
  \item{column_header}{
character vector containing the header of the column containing the strings to be filtered
  }
  \item{orthography_type}{
  contains = partial/total matching - all words containing x

  begins = partial matching - all words begining with x

  ends = partial matching - all words ending with x
  }
  \item{orthography_filter}{
pattern to be filtered
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
new_filtering = filter_strings(data_set = input_data,
                               column_header = 'phrase',
                               orthography_type = 'contains',
                               orthography_filter = c('tev'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
