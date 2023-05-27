\name{blk.isConstant}
\alias{blk.isConstant}
\alias{blk.isVarying}
\title{Check Whether a Variable is Constant or Varying Withing Blocks}
\description{

  This function checks whether a variable is constant or varying
  withing blocks of a \code{\link{block-format}} data set.  A variable
  is constant if it takes the same value at each position within a
  block.  It is varying if it takes more than one distinct value
  within a block.

}
\usage{
blk.isConstant(x, id, ind = NULL)
blk.isVarying(x, id, ind = NULL)
}
\arguments{
  \item{x}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{ind}{A vector of logicals used to filter the values of \code{x}.}
}
\value{
    A logical.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
    \itemize{
    \item \code{\link{block-format}}
    \item \code{\link{blk.applyNto1}}
    \item \code{\link{nunique}}
    }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

blk.isConstant(Wt,    asID(Subject))     # TRUE
blk.isConstant(Apgar, asID(Subject))     # TRUE
blk.isConstant(dose,  asID(Subject))     # FALSE
blk.isConstant(dose,  asID(Subject), ind=(!is.na(dose) & time > 0))   # TRUE

detach(dat)

}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
