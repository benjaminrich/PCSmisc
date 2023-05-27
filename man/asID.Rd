\name{asID}
\alias{asID}
\title{Make a Valid Block Format ID From One or More Vectors}
\description{
  A \code{\link{block-format}} ID is constructed from one or more atomic vectors.  If more
  than one atomic vector is supplied, then a compound ID is constructed by concatenating
  the values together.
}
\usage{
asID(id, ..., sep="|")
}
\arguments{
  \item{id}{An atomic vector.}
  \item{...}{More atomic vectors.}
  \item{sep}{A character to use as a separator when forming a compound ID.}
}
\value{
  A valid block format ID, i.e. a factor \code{x} such that \code{\link{is.unsorted}(x)} is \code{FALSE}.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{block-format}}
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects

attach(dat)

is.unsorted(Subject)           # TRUE  - the original column is NOT sorted
is.unsorted(asID(Subject))     # FALSE - the ID is sorted

all(as.character(Subject) == as.character(asID(Subject)))  # TRUE - textually they are the same

cbind(ID=asID(Subject), dat)

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
