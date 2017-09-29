\name{blk.noninformativeDose}
\alias{blk.noninformativeDose}
\title{Flag Non-informative Dose Events}
\description{
    This function will flag doses that are non-informative in a PK data set
    because they either occur after the last non-dose event or they precede a
    dose given at steady state with no non-dose events occurring between the
    two.
}
\usage{
blk.noninformativeDose(id, dose.ind, ss.ind = NULL)
}
\arguments{
    \item{id}{A valid \code{\link{block-format}} ID.}
    \item{dose.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates rows corresponding to dose events.}
    \item{ss.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates doses given at steady state.}
}
\details{

    This function operates on a PK data set in \code{\link{block-format}}.
    Rows that corresponding to dose events are
    indicated by \code{dose.ind}, and rows that correspond to doses taken at
    steady-state are indicated by \code{ss.ind}.  A steady-state event renders
    any dose events that precede it with no intervening non-dose events
    non-informative.  Doses that occur at the end of a block (i.e. after the last
    non-dose event) are also considered non-informative.
    Non-informative dose events can effectively be removed from the data without
    affecting the analysis of these data.

}
\value{
    A logical vector in \code{\link{block-format}} with respect to \code{id} indicating dose events
    that are non-informative.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
    \itemize{
    \item \code{\link{block-format}}
    \item \code{\link{blk.isSteadyState}}
    }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, INTERDOSE.TIME=blk.intereventTime(time, asID(Subject), !is.na(dose)),
           IS.SS=blk.isSteadyState(time, asID(Subject), !is.na(dose), 12, 0.301, 60, dose),
           FLAG=blk.noninformativeDose(asID(Subject), !is.na(dose), blk.isSteadyState(time, asID(Subject), !is.na(dose), 12, 0.301, 60, dose)))

detach(dat)

}
\keyword{ utilities }
