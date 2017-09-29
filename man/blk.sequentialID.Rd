\name{blk.sequentialID}
\alias{blk.sequentialID}
\title{Create a Sequential ID}
\description{
  A new ID that numbers individuals sequentially starting at 1
  is generated from an existing ID.
}
\usage{
blk.sequentialID(id)
}
\arguments{
  \item{id}{A valid \code{\link{block-format}} ID.}
}
\details{

  An ID is a factor that designates blocks in a
  \code{\link{block-format}} data set.  This function takes an
  existing ID and generates a new one that preserves the same
  structure but is numeric and sequential starting at 1.
  Thus, this is a means of transforming an ID that is desciptive and
  alphanumeric into one that can be used with NONMEM.

}
\value{
  A valid \code{\link{block-format}} ID.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \code{\link{block-format}}
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, newID=blk.sequentialID(asID(Subject)))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
