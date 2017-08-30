\name{tg_summary}
\alias{tg_summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TextGrid summary
%%  ~~function to do ... ~~
}
\description{
Gives TextGrid summary
}
\usage{
tg_summary(incoming_list, interval_label = 'all_labels')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{incoming_list}{
List containing the path and TextGrid names
}
  \item{interval_label}{
    Specifies which labels will be summarised
}

}
\details{
Output:

List containing the summary of the TextGrids
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
TextGrid_summary = tg_summary(fl_lst)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
