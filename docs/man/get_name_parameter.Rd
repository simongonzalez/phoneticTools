\name{get_name_parameter}
\alias{get_name_parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gets string section
%%  ~~function to do ... ~~
}
\description{
Gets the data specifications from a string name
}
\usage{
get_name_parameter(complete_name, name_separator, specify_location)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{complete_name}{
full string containing the parameters to be extracted
  }
  \item{name_separator}{
   separator character in the string
  }
  \item{specify_location}{
    specifies the location of the parameters to be extracted
  }


}
\details{
Output:

List() containing the path and names of the files in the folder
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
string_section = get_name_parameter('speaker_JaneDoe_f_DoverSchool_sample', '_', c(2,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
