\name{folder_content}
\alias{folder_content}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Folder Content
%%  ~~function to do ... ~~
}
\description{
Gets the folder content of the location specified
}
\usage{
folder_content(folder, format_type = 'all_formats', separate_types = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
folder location
}
  \item{format_type}{
    specifies whether the files should be separated or not in the list()
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
get_folder_content = folder_content('data', c('wav', 'tg'), separate_types = F)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
