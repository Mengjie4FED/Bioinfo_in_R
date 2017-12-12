\name{wordsDictionary}
\alias{wordsDictionary}
\alias{getFrequency}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create a wordsDictionary Object for txt File.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wordsDictionary(fileName, removeStopWords = TRUE)
getFrequency(object, word, type = "frequency")
plot(object, yType = "frequency", minFeq = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{fileName}{
%%     ~~Describe \code{fileName} here~~
  An URL, indicating the location of the target txt file.
  }
  
  \item{removeStopWords}{
%%     ~~Describe \code{removeStopWords} here~~
  Logical value, indicating whether to remove stop words in dictionary. The default valur is TRUE.
  }
  
  \item{object}{
  A wordsDictionary object.
  }
  
  \item{word}{
  A string, indicating target word you want to find in dictionary.
  }
  
  \item{type}{
  "frequency" or "ratio", indicating the type of return value. The default value is "frequency".
  }
  
  \item{yType}{
  "frequency" or "ratio", indicating the type of value in y axis. The default value is "frequency".
  }
  
  \item{minFeq}{
  A number, indicating the minimal frequency, which is used to select words above the frequency to be plotted. The default value is 1.
  }
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{wordsDictionary} returns a list, you may use $ to access the element in a wordsDictionary object.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{wordsDictionary}{returns a list. \code{bagOfWords}, {unique words in txt file.} \code{dictionary}, {a dictionary generated from the txt file, implemented by a matrix.}}

\code{getFrequency} returns the frequency of the target word.

\code{plot} draws a plot of the dictionary.
}

\author{
%%  ~~who you are~~
Mengjie Wang
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

