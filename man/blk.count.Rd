\name{blk.count}
\alias{blk.count}
\title{Blockwise Count}
\description{
  Given a data set in \code{\link{block-format}}, determine the block
  sizes or count the number of rows in each block that matche a given
  criterion.
}
\usage{
  blk.count(id, id2=id, ind = NULL)
}
\arguments{
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{id2}{A valid \code{\link{block-format}} ID for the return value, or \code{NULL}}
  \item{ind}{A vector of logicals specifying the rows that satisfy the criterion.}
}
\details{

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
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.singleValue}} and \code{\link{blk.repeatValue}}
  \item \code{\link{tapply}}
  }
}
\examples{

### The size of each block (example)
id <- factor(c("A", "A", "C", "C", "C"), levels=c("A", "B", "C"))
blk.count(id, NULL)  # Note that "B" is a valid ID.  The corresponding block has size zero.

require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
dat$ID <- asID(dat$Subject)

attach(dat)

# Count the number of times concentration was observed for each subject
blk.count(ID, NULL, ind=(!is.na(conc)))

# Count the number of times concentration was observed for each
# subject and repeat it on each row of the data.frame
dat$count.conc <- blk.count(ID, ind=(!is.na(conc)))
dat

detach(dat)

}
\keyword{ utilities }

% vim: tw=70 sw=2
