\name{blk.noninformativeDose}
\alias{blk.noninformativeDose}
\title{Flag Non-informative Dose Events}
\description{
    This function will flag doses that are non-informative in a PK data set
    because they either occur after the last observation event or they precede a
    dose given at steady state with no observation events occurring between the
    two.
}
\usage{
blk.noninformativeDose(id, dose.ind, ss.ind = NULL, obs.ind = !dose.ind)
}
\arguments{
    \item{id}{A valid \code{\link{block-format}} ID.}
    \item{dose.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates rows corresponding to dose events.}
    \item{ss.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates doses given at steady state.}
    \item{obs.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates rows corresponding to observation events.}
}
\details{

    This function operates on a PK data set in \code{\link{block-format}}.
    Rows that corresponding to dose events are
    indicated by \code{dose.ind}, and rows that correspond to doses taken at
    steady-state are indicated by \code{ss.ind}.  A steady-state event renders
    any dose events that precede it with no intervening observation events
    non-informative.  Doses that occur at the end of a block (i.e. after the last
    observation event) are also considered non-informative.
    Non-informative dose events can effectively be removed from the data without
    affecting the analysis of these data.

}
\value{
    A logical vector in \code{\link{block-format}} with respect to \code{id} indicating dose events
    that are non-informative.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

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

dat2 <- data.frame(id=rep(1:2, each=10), dose=0, obs=0)
dat2$dose[c(1:3, 7:8, 12:13, 19:20)] <- 1
dat2$obs[c(5:6, 15:18)] <- 1
dat2$flag <- with(dat2, 1*blk.noninformativeDose(asID(id), dose.ind=(dose==1), obs.ind=(obs==1)))
dat2

}
\keyword{ utilities }
