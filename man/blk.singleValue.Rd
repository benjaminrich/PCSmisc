\name{blk.singleValue}
\alias{blk.singleValue}
\alias{blk.repeatValue}
\title{Extract a Single Value for Each Block in a Block Data Set.}
\description{
  Extract a single value for each block in a
  \code{\link{block-format}} data set.
}
\usage{
blk.singleValue(x, id, ind = NULL, select = c("first", "last"), fill = NA)
blk.repeatValue(x, id, id2=id, ind = NULL, select = c("first", "last"), fill = NA)
}
\arguments{
  \item{x}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID for the input \code{x}.}
  \item{id2}{A valid \code{\link{block-format}} ID for the return value.}
  \item{ind}{A vector of logicals used to filter the values of \code{x}.}
  \item{select}{When more than one value exists, which one to select.  See details.}
  \item{fill}{A value to use when no other value is appropriate. See details.}
}
\details{
  These functions allow one to extract a single value per block in a
  \code{\link{block-format}} data set.  This can be useful in many
  contexts.  If the values of \code{x} in each block are not unique,
  then a specific value needs to be determined.  The indicator vector
  \code{ind} can be used to filter out specific rows that contain
  values of interest.  If there is more than one value, then
  \code{select} is used to choose either the first or the last
  (according to the ordering of \code{x}).  If, for a given block,
  there is no value at all, either because none of the rows matched
  the \code{ind} criteria or because the block is of size zero (see
  \code{\link{block-format}}), then the value of \code{fill} is used
  for that block.

  Both \code{blk.singleValue} and \code{blk.repeatValue} determine a
  unique value for each block.  The difference between them is how
  many times each value is repeated in the result.  The vector
  returned by \code{blk.singleValue} has length equal to the number of
  blocks in \code{id}, i.e. each value appears exactly once.  For
  \code{blk.repeatValue}, each value is repeated the appropriate
  number of times so that the result is a vector in block format with
  respect to \code{id2}.  Thus, \code{blk.repeatValue} can effectively
  be used to perform a simple ``left outer join'' (or ``merge'')
  operation on a single variable (see example).

  Neither of \code{levels(id)} and \code{levels(id2)} need be a proper
  subset of the other.  For levels of \code{id2} (resp. \code{id}) that
  are not levels of \code{id} (resp. \code{id2}), the corresponding blocks
  are assumed to be of size zero in \code{id} (resp. \code{id2}).

  It is also possible to use \code{blk.repeatValue} in a different
  way, without specifying \code{id}.  In that case, \code{x} must
  have length equal to \code{nlevels(id2)}, i.e. it must contain a
  unique value for each possible value of \code{id2}, and the
  correspondence of values to ID's is taken from their respective
  ordering.  Then, each value in \code{x} is repeated the number of
  times that the corresponding ID appears in \code{id2}.
  This is equivalent to \code{blk.repeatValue(x, id=asID(levels(id2)), id2, ...)}.
}
\value{
  \code{blk.singleValue} returns a vector containing one value for
  each level of \code{id}.

  \code{blk.repeatValue} returns a vector in
  \code{\link{block-format}} with respect to \code{id2}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{tapply}}
  \item \code{\link{merge}}
  \item \code{\link{duplicated}}
  }
}
\examples{

# EXAMPLE 1
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
dat$id <- asID(dat$Subject)

attach(dat)

# A single row per subject
data.frame(id=levels(id),
           Wt=blk.singleValue(Wt, id),
           Apgar=blk.singleValue(Apgar, id),
           final.dose=blk.singleValue(dose, id, ind=(!is.na(dose)), select="last"))

# Repeat a single value on each row for each subject
cbind(dat, data.frame(
  first.dose=blk.repeatValue(dose, id, ind=(!is.na(dose))),
  final.dose=blk.repeatValue(dose, id, ind=(!is.na(dose)), select="last")
))

detach(dat)

### Merging a time-fixed covariate (simple left outer join)
### -------------------------------------------------------

# Suppose subjects 1 and 2 are Male, and Subject 4 is Female, but the
# gender of subject 3 is not specified.
gender <- data.frame(
    id=factor(c(1, 2, 4), levels=levels(dat$id)),   # Note: keeping the same factor levels helps
    gender=c("Male", "Male", "Female"))

gender

# Now, 'merge' the gender with the rest of the data.
# Since subject 3 is absent, it gets the value of 'fill', i.e. NA.
dat$gender <- blk.repeatValue(gender$gender, gender$id, dat$id)
dat

# Still returns 4 values:
blk.singleValue(gender$gender, gender$id)

### The other way of using blk.repeatValue (without specifying id)
### --------------------------------------------------------------

letter <- LETTERS[1:nlevels(dat$id)]  # Exactly one value per id
cbind(dat,    letter=blk.repeatValue(c("A", "B", "C", "D"), id2=dat$id))
cbind(gender, letter=blk.repeatValue(c("A", "B", "C", "D"), id2=gender$id))

# EXAMPLE 2
id <- gl(4, 4)
x <- LETTERS[1:16]
y <- Sys.time() + 1:16

data.frame(
    id       = levels(id),
    first.x  = blk.singleValue(x, id),
    first.y  = blk.singleValue(y, id))

data.frame(
    id       = id,
    x        = x,
    first.x  = blk.repeatValue(x, id),
    y        = y,
    first.y  = blk.repeatValue(y, id))

target.id <- gl(4, 6)
data.frame(
    id       = target.id,
    first.x  = blk.repeatValue(x, id, target.id),
    first.y  = blk.repeatValue(y, id, target.id))


}
\keyword{ utilities }

% vim: tw=70 sw=2
