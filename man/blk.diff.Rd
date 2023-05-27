\name{blk.diff}
\alias{blk.diff}
\alias{blk.intereventTime}
\title{Blockwise Lagged Differences}
\description{
  Computes lagged difference within blocks and on selected elements.
}
\usage{
blk.diff(x, id, ind = NULL, lag = 1, fill = NA, diff.op = "-", ...)
blk.intereventTime(time, id, ind = NULL, lag = 1, fill = NA, diff.op = difftime.default, ...)
}
\arguments{
  \item{x,time}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{ind}{A logical vector that designates a subset of \code{x}.  By default all are included.}
  \item{lag}{An integer specifying the lag.}
  \item{fill}{A value to use when no other value is appropriate.}
  \item{diff.op}{A function that subtracts one value from another.}
  \item{...}{Further arguments passed to \code{diff.op}.}
}
\details{

  These functions operate on data sets in \code{\link{block-format}}.
  Essentially, the standard \code{\link{diff}} function is applied
  within each block, except that a function \code{diff.op} can be
  specified for doing the subtraction.  Additionally, a subset on
  which to perform the operation can be selected with \code{ind}.  For
  elements that are not selected the corresponding result is given by
  \code{fill}.  The first \code{lag} elements of each block are the
  result are also assigned the value \code{fill} so that the result is
  in \code{\link{block-format}} with respect to \code{id}.

  For time values, \code{blk.intereventTime} is an alias with a more
  descriptive name and a different default \code{diff.op}.

}
\value{
  A vector in \code{\link{block-format}} with respect to \code{id}
  containing the differenced values.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{diff}}
  \item \code{\link{deltat}}
  }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, INTERDOSE.TIME=blk.intereventTime(time, asID(Subject), ind=!is.na(dose)))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
