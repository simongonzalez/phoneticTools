\name{classify_words}
\alias{classify_words}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{POS Word Classification
%%  ~~function to do ... ~~
}
\description{
Classifies words into Parts of Speech, syllable, and characater number
}
\usage{
classify_words(data_set = NULL, column_header = NULL,
               POS_tag = T, POS_tag_long = F, POS_tag_simplified = F,
               syllable_number = T, character_number = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
dataset (data.frame) containing the words to be classified
}
  \item{column_header}{
character vector containing the header of the column containing the words to be classified
}
  \item{POS_tag}{
boolean for showing the generalised POS tag, e.g. 'CC','CD','DT','EX'
  }
  \item{POS_tag_long}{
boolean for showing the long POS tag, e.g. 'Coordinating conjunction','Cardinal number','Determiner','Existential there'
  }
  \item{POS_tag_simplified}{
boolean for showing the simplified POS tag, e.g. 'conjunction','number', 'determiner','there'
  }
    \item{syllable_number}{
  boolean for showing the number of syllables per word
  }
    \item{character_number}{
    boolean for showing the number of characters per word
   }


}
\details{
Output:

Exports the same dataset with new columns containing the word classifications
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
new_classififcaion =  classify_words(data_set = input_data,
                      column_header = 'word',
                      POS_tag = T,
                      POS_tag_long = T,
                      POS_tag_simplified = T,
                      syllable_number = T,
                      character_number = T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
