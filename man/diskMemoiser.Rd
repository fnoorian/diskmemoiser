\name{diskMemoiser}
\alias{diskMemoiser}
\alias{diskMemoizer}

\title{Disk Memoiser}
\description{
Performs memoisation of arbitrary functions to disk to speed up repeated calls to those functions.
}
\usage{
diskMemoizer(fun, memo.dir = "./memo")
diskMemoiser(fun, memo.dir = "./memo")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
    An arbitrary function.
  }
  \item{memo.dir}{
    An optional directory to store the memoised results. If not specified, defaults to "./memo"
  }
}
\details{
  This function defines a memoisation wrapper which saves the results of the function to disk. 
  If the function is called with the same parameters it retrieves the previous result and returns it.
  Note that this only makes sense if the function is a deterministic and not dependent on external variables. Use carefully!
}

%% ~put references to the literature/web site here ~ 
\references{
%\url{http://cran.r-project.org/web/packages/R.cache}
\url{http://cran.r-project.org/web/packages/memoise}
}
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
% %% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%}
\examples{
myfunction.slow <- function(x, y) {
  return(x + sin(y))
}

myfunction <- diskMemoiser(myfunction.slow)
result.slow <- myfunction(3, 5)
result.fast <- myfunction(3, 5)

# The memoization directory defaults to ./memo, but can be reassigned:
myfunction <- diskMemoiser(myfunction.slow, memo.dir = "./temp")
result.slow <- myfunction(7, 11)
result.fast <- myfunction(7, 11)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
