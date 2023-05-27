\name{blk.findConsecutive}
\alias{blk.findConsecutive}
\title{Finds Consecutive Rows With a Common Property}
\description{
  Groups of consecutive rows are identified, where each row in the
  group has a certain property.
}
\usage{
blk.findConsecutive(id, ind, min.consec = 2)
}
\arguments{
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{ind}{A logical vector in \code{\link{block-format}} with
  respect to \code{id} that designates rows in the data set for which the property holds.}
  \item{min.consec}{An integer specifying the minumum size of a group.}
}
\details{

  This function operates on data sets in \code{\link{block-format}}.
  Some rows have a certain property as indicated by \code{ind}.  This
  function identifies groups of rows that all possess the property,
  are consecutive and have a minimum size \code{min.consec}.  All such
  groups of rows identified are assigned a unique integer ID.  All
  remaining rows are given the value 0.

}
\value{
  An integer vector in \code{\link{block-format}} with respect to \code{id}
  containing a unique group ID for rows belonging to identified groups, and 0 otherwise.
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

cbind(dat, CONSEC=blk.findConsecutive(asID(Subject), ind=!is.na(dose), min.consec=5))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
