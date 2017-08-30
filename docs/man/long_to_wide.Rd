\name{long_to_wide}
\alias{long_to_wide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Long to wide data
%%  ~~function to do ... ~~
}
\description{
Converts long to wide data
}
\usage{
long_to_wide(data_frame = NULL, set_columns = NULL,
             new_column_names = NULL, points_column = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_frame}{
dataset (data.frame) to convert from long to wide format
}
  \item{set_columns}{
    character vector containing the header of the columns to be split into different columns
}
  \item{new_column_names}{
    character vector containing the names of the new columns
  }
  \item{points_column}{
    character vector containing the name of the points column
  }

}
\details{
Output:

Exports the same dataset with the new columns
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
long_data = wide_to_long(data_set = input_data,
                         set_columns = 'F1.35. F1.65.,F2.35. F2.65.',
                         new_column_names = 'F1s F2s')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
