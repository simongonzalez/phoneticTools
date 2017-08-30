\name{filter_labels}
\alias{filter_labels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Label filtering
%%  ~~function to do ... ~~
}
\description{
Filters labels of any column based on user input parameters
}
\usage{
filter_labels(data_set = NULL, column_header = NULL,
              keep_list = NULL, omit_list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the columns to be filtered
}
  \item{column_header}{
character variable containing the header of the column to have content filtered
}
  \item{keep_list}{
character vector containing the elements to be kept in the column
  }
  \item{omit_list}{
character vector containing the elements to be left out in the column
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
new_filtering = filter_labels(data_set = input_data,
                column_header = 'segment',
                keep_list = c('i:', 'u', 'U'),
                omit_list = NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
