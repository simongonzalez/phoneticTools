\name{smooth_trajectory}
\alias{smooth_trajectory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Smooths Trajectories
%%  ~~function to do ... ~~
}
\description{
Smooths Trajectories
}
\usage{
smooth_trajectory(data_frame = NULL, set_columns = NULL,
                  smooth_method = NULL, smooth_parameter = NULL,
                  points_number = NULL, even_number = T, by_column = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_frame}{
dataset (data.frame) containing the values to be smoothed
}
  \item{set_columns}{
    character vector containing the header of the columns containing the values to be smoothed
}
  \item{smooth_method}{
    smooth method to be applied
}
  \item{smooth_parameter}{
    depending on the smooth method, the smooth parameter specifies the amount of smoothness
}
  \item{points_number}{
  number of points in the final trajectory
}
  \item{even_number}{
    applies only to entries with even numbers
}
  \item{by_column}{
  specifies the column with the (time) points
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
smoothed_formants = smooth_trajectory(data_frame = input_data,
                                      set_columns = c('f1', 'f2'), smooth_method = 'dct',
                                      smooth_parameter = 3, by_column = 'extraction_param')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
