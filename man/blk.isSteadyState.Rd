\name{blk.isSteadyState}
\alias{blk.isSteadyState}
\title{Flag Dose Events Occuring Under Steady-State Conditions}
\description{
    This function will flag dose events that occur under steady-state conditions in a PK data set that contains
    all single dose events.
}
\usage{
blk.isSteadyState(time, id, dose.ind, dose.interval, tol, min.time.ss, dose = NULL, diff.op = difftime.default)
}
\arguments{
    \item{time}{A vector of event times in \code{\link{block-format}} with respect to \code{id}.}
    \item{id}{A valid \code{\link{block-format}} ID.}
    \item{dose.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates rows corresponding to dose events.}
    \item{dose.interval}{The dosing interval.}
    \item{tol}{A tolerance for the actual length of the dosing intervals.  See details.}
    \item{min.time.ss}{Steady-state is achieved when the same dose is taken at
        constant intervals for at least this amount of time.}
    \item{dose}{Vector of dose amounts in \code{\link{block-format}} with respect to \code{id}.
        If it is \code{NULL} (the default), then assume that all doses are the same.}
    \item{diff.op}{A function that subtracts one time from another.}
}
\details{

    This function operates on a PK data set in \code{\link{block-format}}.
    Event times are specified by \code{time} and rows that corresponding to dose events are
    indicated by \code{dose.ind}.

    A steady-state condition is achieved if the same dose is taken at constant
    intervals for a sufficient length of time.  The length of time required to
    reach steady state will in general vary between compounds and between
    individuals, but a rule of thumb is that the time should at minimum exceed 5 to 7
    times the half-life of the compound.  The time deemed necessary is
    specified using \code{min.time.ss}.  Pharmacokinetic analysis can be
    simplified if it is known that certain dose events occur under steady-state
    conditions.

    For steady state, doses must be given at constant intervals.  The dosing
    interval is specified by \code{dose.interval}.  The actual times between
    doses may vary, however.  Using the argument \code{tol} a tolerance can be
    specified.  If the actual time interval between two consecutive doses is
    within \code{tol} of \code{dose.interval}, then these doses can be part of
    a repeated-dose sequence that leads to steady-state conditions.

}
\value{
    A logical vector in \code{\link{block-format}} with respect to \code{id}
    indicating dose events occurring at steady state according to the criteria
    specified.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
    \itemize{
    \item \code{\link{block-format}}
    \item \code{\link{blk.intereventTime}}
    \item \code{\link{blk.noninformativeDose}}
    \item \code{\link{difftime.default}}
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

% vim: tw=70 sw=2 ts=2 et :
