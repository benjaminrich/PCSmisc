\name{blk.indicators}
\alias{blk.indicators}
\alias{blk.firstOnly}
\alias{blk.lastOnly}
\alias{blk.onlyFirst}
\alias{blk.onlyLast}
\alias{blk.untilFirst}
\alias{blk.firstOnwards}
\alias{blk.untilLast}
\alias{blk.lastOnwards}
\title{Blockwise Indicator Functions}
\description{
  Functions that return indicators (logical vectors) indicating rows in a data
  set that match a criterion.
}
\usage{
blk.firstOnly(id, ind = NULL)
blk.lastOnly(id, ind = NULL)
blk.onlyFirst(id, ind = NULL)
blk.onlyLast(id, ind = NULL)
blk.untilFirst(id, ind, include.first = TRUE, fill = FALSE)
blk.firstOnwards(id, ind, include.first = TRUE, fill = FALSE)
blk.untilLast(id, ind, include.last = TRUE, fill = FALSE)
blk.lastOnwards(id, ind, include.last = TRUE, fill = FALSE)
}
\arguments{
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{include.first}{A logical.  Should the first matching row itself be included?}
  \item{include.last}{A logical.  Should the last matching row itself be included?}
  \item{fill}{A value to use when no other value is appropriate.}
}
\details{

  These functions operate on data sets in \code{\link{block-format}}.

  \code{blk.firstOnly} selects the first element of each block for which \code{ind} is \code{TRUE}.
  \code{blk.onlyFirst} is an alias.

  \code{blk.lastOnly} selects the last element of each block for which \code{ind} is \code{TRUE}.
  \code{blk.onlyLast} is an alias.

  Setting \code{ind} to \code{NULL} is equivalent to setting \code{ind =
  rep(TRUE, length(id))}, so the first or last row of each block is selected.
  
  \code{blk.untilFirst} selects all elements from the start of each block until
  the first element of the block for which \code{ind} is \code{TRUE}.  Whether
  or not that element itself is selected is controlled by \code{include.first}.
  \code{blk.firstOnwards} is similar, but it selects all rows from the first
  element of each block for which \code{ind} is \code{TRUE} to the end of the
  block.  \code{blk.untilLast} and \code{blk.lastOnwards} are similar,
  selecting all elements from the start of the block to the last element for
  which \code{ind} is \code{TRUE}, and all elements from this one to the end of
  the block repectively (including the last element for which \code{ind} is
  \code{TRUE} if \code{include.last} is FALSE, and excluding it otherwise).

}
\value{
    An indicator vector (logical) \code{\link{block-format}} with respect to \code{id} that indicates the elements selected.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \code{\link{block-format}}
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,] # First 4 subjects
attach(dat)

cbind(dat, IND=blk.firstOnly(asID(Subject), !is.na(conc)))
cbind(dat, IND=blk.lastOnly(asID(Subject), !is.na(conc)))
cbind(dat, IND=blk.untilFirst(asID(Subject), !is.na(conc)))
cbind(dat, IND=blk.untilFirst(asID(Subject), !is.na(conc), FALSE))
cbind(dat, IND=blk.firstOnwards(asID(Subject), !is.na(conc)))
cbind(dat, IND=blk.firstOnwards(asID(Subject), !is.na(conc), FALSE))
cbind(dat, IND=blk.untilLast(asID(Subject), !is.na(dose)))
cbind(dat, IND=blk.untilLast(asID(Subject), !is.na(dose), FALSE))
cbind(dat, IND=blk.lastOnwards(asID(Subject), !is.na(dose)))
cbind(dat, IND=blk.lastOnwards(asID(Subject), !is.na(dose), FALSE))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
