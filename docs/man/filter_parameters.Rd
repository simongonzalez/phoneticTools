\name{filter_parameters}
\alias{filter_parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Acoustic values filtering
%%  ~~function to do ... ~~
}
\description{
Filters acoustic values based on user input parameters
}
\usage{
filter_parameters(data_set = NULL, parameter_type = NULL, parameter_header = NULL,
                  filter_parameter = NULL, parameter_subset = NULL,
                  std_threshold = 1, std_every = NULL, data_subset = NULL,
                  delete_uneven = T, delete_uneven_columns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the columns of acoustic values to be filtered
}
  \item{parameter_type}{
character variable containing the acoustic parameter to be filtered
  }
  \item{parameter_header}{
character variable containing the header of the column to have content filtered
  }
  \item{filter_parameter}{
numeric vector containing the values for filtering, i.e. thesholds or specific values conventions:

  ... 1. One positive values = ONLY observations below or equal that value are kept

  ... 2. One negative values = ONLY observations above that value are kept

  ... 3. Two positive values = ONLY observations between these values are kept
  }
  \item{parameter_subset}{
list containing data subsets for the filtering. If parameter_subset is NULL, all values will be filtered based on the filter_parameter argument.

If the user chooses to have different thresholds for different subsets, a list can be entered specifying thresholds as in the filter_parameter argument, e.g. list('i:' = 500, 'e' = -50, 'u' = c(400, 600))
  }
    \item{std_threshold}{
 Filter by Standard Deviations.

 If one positive number is entered, all Standard Deviations within that threshold will be kept.

 If one negative number is entered, all Standard Deviations out of that threshold will be excluded.
  }
    \item{std_every}{
    If std_threshold is selected (not NULL) and std_every is left NULL, Standard Deviations are calculated based on all the observations of the data, this is, all rows.

    If there is more than one observation point for each token (i.e. more than one point in trajectories), then tokens will have the same Standard Deviations repeated. This will affect the Standard Deviation value.

    Thus if std_every is not NULL, then the Standard Deviation values will based on the every number of rows entered.
}
    \item{data_subset}{
    If std_threshold is selected (not NULL), Standard Deviations are calculated based on the data_subset input.

    One or more subset columns can be selected. If left NULL, the Standard Deviation is calculated based on all values without any groupings.
}
    \item{delete_uneven}{
boolean for determining whether uneven entries after filtering will be deleted, e.g. if each token MUST have five (5) entries but the filtering left some tokens with three (3) or four (4) entries. For the latter tokens, all entries with eneven data are deleted
  }
    \item{delete_uneven_columns}{
    If delete_uneven == TRUE, character vector containing the column headers to be inspected for uneven number of entries
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
new_filtering =  filter_parameters(data_set = input_data,
                                   parameter_type = 'pitch',
                                   parameter_header = 'pitch',
                                   filter_parameter = c(200),
                                   delete_uneven = T,
                                   delete_uneven_columns = c('overall_token', 'extraction_param'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
