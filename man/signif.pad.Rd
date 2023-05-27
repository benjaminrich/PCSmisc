\name{signif.pad}
\alias{signif.pad}
\title{Round Numbers to Specified Significant Digits with 0-Padding}
\description{
  A utility function to round numbers to a specified number of
  significant digits.  Zeros are kept if they are significant.
}
\usage{
signif.pad(x, digits = 3, round.integers = FALSE)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{digits}{An interger specifying the number of significant digits to keep.}
  \item{round.integers}{Should rounding be limited to digits to the right of the decimal point?}
}
\value{
  A character vector containing the rounded numbers.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{signif}}
  \code{\link{formatC}}
  \code{\link{prettyNum}}
  \code{\link{format}}
}
\examples{

x <- c(0.9001, 12345, 1.2, 1., 0.1)
signif.pad(x, digits=3)
signif.pad(x, digits=3, round.integers=TRUE)

# Compare:
as.character(signif(x, digits=3))
format(x, digits=3, nsmall=3)
prettyNum(x, digits=3, drop0trailing=TRUE)
prettyNum(x, digits=3, drop0trailing=FALSE)

# This is very close.
formatC(x, format="fg", flag="#", digits=3) 
formatC(signif(x, 3), format="fg", flag="#", digits=3)

# Could always remove the trailing "."
sub("[.]$", "", formatC(x, format="fg", flag="#", digits=3))

}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
