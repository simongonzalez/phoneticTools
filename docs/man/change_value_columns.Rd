\name{change_value_columns}
\alias{change_value_columns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Changes column values
%%  ~~function to do ... ~~
}
\description{
Changes column values based on user input parameters
}
\usage{

change_value_columns(data_set = NULL, column_header = NULL,
                     current_values = NULL, new_values = NULL,
                     override_column_values = F, new_column_header = NULL,
                     delete_column_header = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the values to be changed
}
  \item{column_header}{
character vector containing the header of the column with the values to be changed
}
  \item{current_values}{
target values
}
  \item{new_values}{
new values
}

  \item{override_column_values}{
boolean: if TRUE, the new values overwrite the old labels in the same column.
}
  \item{new_column_header}{
if override_column_values == FALSE, a new column is created with the specified label
}
  \item{delete_column_header}{
specifies the header of the column to be deleted
}


}
\details{
Output:

Exports the same dataset with the values changed
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
