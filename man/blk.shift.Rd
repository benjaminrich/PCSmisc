\name{blk.shift}
\alias{blk.shift}
\title{Shift Values Respecting Block Boundaries}
\description{
  Shifts values up or down by a given number of positions within
  blocks and on selected elements.
}
\usage{
blk.shift(x, id, shift.by = 1, ind = NULL, fill = NA)
}
\arguments{
  \item{x}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{shift.by}{An integer specifying the number of positions to shift by. Can be positive (shift down) or negative (shift up).}
  \item{ind}{A logical vector that designates a subset of \code{x} to use.  By default all are selected.}
  \item{fill}{A value to use when no other value is appropriate.}
}
\details{

  This function operates on data sets in \code{\link{block-format}}.
  The number of positions by which to shift by may be positive or
  negative, with positive indicating downwards in the data set and
  negative upwards.  Additionally, a subset on which to perform the
  operation can be selected with \code{ind}.  For elements that are
  not selected the corresponding result is given by \code{fill}.
  Shifting creates `gaps' at the start (or end if \code{shift.by} is
  negative) of a block.  The gaps are filled with the value
  \code{fill} so that the result is in \code{\link{block-format}} with
  respect to \code{id}.

}
\value{
  A vector in \code{\link{block-format}} with respect to \code{id}
  containing the shifted values.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{lag}}
  }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, SHIFT=blk.shift(time, asID(Subject), 2))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
