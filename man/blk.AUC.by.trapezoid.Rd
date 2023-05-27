\name{blk.AUC.by.trapezoid}
\alias{blk.AUC.by.trapezoid}
\title{Compute Area Under a Curve by the Trapezoid Rule.}
\description{
  Compute area under a curve (AUC) by the trapezoid rule within
  each block of a \code{\link{block-format}} data set.
}
\usage{
blk.AUC.by.trapezoid(x, y, id, id2 = id, ind = NULL, fill = NA, diff.op = "-", ...)
}
\arguments{
  \item{x,y}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{id2}{A valid \code{\link{block-format}} ID for the return value, or \code{NULL}}
  \item{ind}{A logical vector that designates a subset of points.  By default all points are included.}
  \item{fill}{A value to use when no other value is appropriate.}
  \item{diff.op}{A function that computes differences in \code{x}.}
  \item{...}{Further arguments passed to \code{diff.op}.}
}
\details{

  This function operates on data sets in \code{\link{block-format}}.
  Within each block,
  the points on a curve are determined by \code{x} and \code{y}.  A
  subset of points to include can be specified with \code{ind}.
  Then, within each block, AUC is computed by the trapezoid rule.

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
data(pkstudy.PC)

df <- with(pkstudy.PC, data.frame(
    id       = asID(subjid),
    datetime = strptime(pcdtc, "\%Y-\%m-\%dT\%H:\%M", tz="UTC"),
    reltime  = NA,
    conc     = ifelse(grepl("^BQL", pcstresc), 0, pcstresn)))

# Simulate missing values
df$conc[c(3, 25)] <- NA

df$reltime <- with(df, blk.relativeTime(datetime, id))

# EXAMPLE 1 - id2=NULL
with(df, blk.AUC.by.trapezoid(x=reltime, y=conc, id=id, id2=NULL, ind=!is.na(conc)))

# EXAMPLE 2 - id2=id (default)
df$auc <- with(df, blk.AUC.by.trapezoid(x=reltime, y=conc, id=id, ind=!is.na(conc)))

# EXAMPLE 3 - use datetime directly and speficy diff.op
df$auc2 <- with(df, blk.AUC.by.trapezoid(x=datetime, y=conc, id=id, ind=!is.na(conc), diff.op=difftime.hours))

# See results:
head(df, 30)
}
\keyword{ utilities }

% vim: tw=70 sw=2
