\name{blk.applyNto1}
\alias{blk.applyNto1}
\alias{blk.applyNtoN}
\title{Apply a Function to Each Block.}
\description{
  Apply a function to each block in a \code{\link{block-format}} data set
  and return the result in a suitable format.
}
\usage{
blk.applyNto1(x, id, apply.fun, ind = NULL, fill = NA, ...)
blk.applyNtoN(x, id, apply.fun, ind = NULL, fill = NA, ...)
}
\arguments{
  \item{x}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID for the input \code{x}.}
  \item{apply.fun}{A function to apply to each block of \code{x}.  See details.}
  \item{ind}{A vector of logicals used to filter the values of \code{x}.}
  \item{fill}{A value to use when no other value is appropriate. See details.}
  \item{...}{Further arguments passed to \code{apply.fun}.}
}
\details{

  These functions operate on data sets in \code{\link{block-format}}.
  The function \code{apply.fun} on each block of \code{x}, similar to
  \code{\link{tapply}}.  The values of \code{x} passed to the function
  can be filtered by using \code{ind} to specify a subset.

  If the objective is to extract specific values from \code{x} rather
  than to use a function to transform the values, then
  \code{\link{blk.singleValue}} or \code{\link{blk.repeatValue}}
  should be used instead.

  For \code{blk.applyNto1}, the function \code{apply.fun} should take
  a vector as its first argument and return a vector of length 1.  If
  the returned vector has length greater than 1, then only the first
  element will be kept and a warning issued.  If the returned vector
  has length 0, then the result for the correponding block is
  \code{NA}, and a warning is issued.

  For \code{blk.applyNtoN}, the function \code{apply.fun} should take
  a vector as its first argument and return a vector.  The length of
  the returned vector will be extended or reduced as necessary to
  obtain a vector matching the length of the function input with
  elements recycled if necessary.  If the length of the input vector
  is not a multiple of the length of the returned vector then a
  warning is issued.  In general, only functions for which the length
  of the returned vector equals to the length of the input or 1
  make sense.

  The result contains the value \code{fill} for blocks of size zero
  (see \code{\link{block-format}}) or for which all \code{ind} equals
  \code{FALSE}.

}
\value{
  \code{blk.applyNto1} returns a vector containing one value for
  each level of \code{id}.

  \code{blk.applyNtoN} returns a vector in
  \code{\link{block-format}} with respect to \code{id}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.singleValue}}, \code{\link{blk.repeatValue}}
  \item \code{\link{tapply}}, \code{\link{by}}, \code{\link{aggregate}}
  }
}
\examples{

require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects

attach(dat)

# Find the maximum concentration observed for each subject
cbind(dat, Cmax=blk.applyNtoN(conc, asID(Subject), max, na.rm=TRUE))

}
\keyword{ utilities }

% vim: tw=70 sw=2
