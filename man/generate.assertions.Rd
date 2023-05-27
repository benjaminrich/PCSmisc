\name{generate.assertions}
\alias{generate.assertions}
\title{Generate Assertions}
\description{
  Use this function to generate simple code asserstions about a
  \code{\link{data.frame}}.
}
\usage{
generate.assertions(dat, max.level.show = 12, digits.compare = 6)
}
\arguments{
  \item{dat}{A \code{\link{data.frame}}.}
  \item{max.level.show}{Maximum number of factor levels for which to
  generate an assertion.}
  \item{digits.compare}{How many decimal digits to include in
  comparisons of numeric data?}
}
\details{
  Assertions are generated about the number of rows and columns in the
  \code{\link{data.frame}}, and about each column it contains.
  An assertion is generated about the class of the column, and the number
  of missing values (\code{NA}).  If the column has a \code{label}
  attribute, then as assertion is generated for that as well.
  Further assertions depend on the \code{class} of the column.
  For \code{factor}s, the number of levels is asserted.  If this does
  not exceed \code{max.level.show} then the \code{levels} are
  asserted as well.  For numeric data, the range (rounded to
  \code{digits.compare} is asserted.
}
\value{
  None (\code{\link{invisible}} `\code{NULL}').
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{stopifnot}}
}
\examples{
\dontrun{
require(nlme)
data(Phenobarb)
sink("Phenobarb_checks.R")
generate.assertions(Phenobarb)
sink()
}
}

\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
