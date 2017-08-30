\name{rename_columns}
\alias{rename_columns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Renames column(s)
%%  ~~function to do ... ~~
}
\description{
Renames the column(s) specified
}
\usage{

rename_columns(data_set = NULL, current_column = NULL, new_column = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the column(s) to be renamed
}
  \item{current_column}{
character vector containing the header(s) of the column(s) to be renamed
}
  \item{new_column}{
character vector containing the name(s) of the new column(s)
}


}
\details{
Output:

Exports the same dataset with the columns renamed
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
