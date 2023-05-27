\name{blk.time.above.threshold}
\alias{blk.time.above.threshold}
\title{Compute Time Above a Threshold by Linear Interpolation.}
\description{
  Compute the time above a threshold by linear interpolation within
  each block of a \code{\link{block-format}} data set.
}
\usage{
blk.time.above.threshold(x, y, threshold, id, id2 = id, ind = NULL, fill = NA, diff.op.x = "-", diff.op.y = "-", ...)
}
\arguments{
  \item{x,y}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{threshold}{A numeric vector of length one specifying the threshold.}
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{id2}{A valid \code{\link{block-format}} ID for the return value, or \code{NULL}}
  \item{ind}{A logical vector that designates a subset of points.  By default all points are included.}
  \item{fill}{A value to use when no other value is appropriate.}
  \item{diff.op.x}{A function that computes differences in \code{x}.}
  \item{diff.op.y}{A function that computes differences in \code{y}.}
  \item{...}{Further arguments passed to \code{diff.op.x} and \code{diff.op.y}.}
}
\details{

  This function operates on data sets in \code{\link{block-format}}.
  Within each block,
  the points on a graph are determined by \code{x} and \code{y}.  A
  subset of points to include can be specified with \code{ind}.
  Then, within each block, the time above \code{threshold} is computed
  by linear interpolation. 

  The parameter \code{id2} determines the return value.
  If \code{id2} is \code{NULL}, the returned vector contains one
  element for each level of \code{id}, with \code{fill} used for
  blocks of size zero.

}
\value{
  If \code{id2} is \code{NULL},
  a vector containing one value for
  each level of \code{id}.  Otherwise,
  a vectors in \code{\link{block-format}} with respect to \code{id2}.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{block-format}}
}
\examples{
threshold <- 5
x0 <- 1:20
y0 <- 0.1 + runif(20, 0, 0.9)
y0[6] <- y0[5]
y0[14] <- y0[11]
y0[c(16)] <- 0
y0[c(2, 3, 4, 7, 8, 9, 15, 16, 18)] <- 0
y0[c(1, 10, 11, 12, 13, 14, 15, 19, 20)] <- -y0[c(1, 10, 11, 12, 13, 14, 15, 19, 20)]
y0[c(1, 10, 19)] <- -y0[c(5, 6, 17)]
y0 <-(1 + y0)*threshold
x <- x0[c(1, 5, 6, 10, 11, 14, 16, 17, 19, 20)]
y <- y0[c(1, 5, 6, 10, 11, 14, 16, 17, 19, 20)]
plot(x0, y0, type="n", ylim=c(0, 2*threshold)); abline(h=threshold, col="red", lwd=3)
points(x0, rep(threshold, length(x0)), col="red", pch=19, cex=1)
lines(x, y, type="b", col="blue", lwd=2)
t <- blk.time.above.threshold(x, y, threshold, id=asID(rep(1, length(x2))), id2=NULL)
stopifnot(t == 7)
}
\keyword{ utilities }

% vim: tw=70 sw=2
