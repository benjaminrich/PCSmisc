\name{blk.addl}
\alias{blk.addl}
\alias{blk.ii}
\title{Count Additional Doses at a Fixed Interval}
\description{
    These are special functions to help reduce multiple dose records
    at a fixed interval to single dose records with an \code{ADDL}
    column for the number of \emph{additional} doses and an \code{II}
    column for the \emph{interdose interval} in the style of NONMEM.
}
\usage{
blk.ii(time, id, dose.ind, diff.op=difftime.default)
blk.addl(ii, id, dose.ind, dose, tol.ii=1e-5, tol.dose=1e-5, min.consec=2, include.last=FALSE, diff.op=difftime.default)
}
\arguments{
    \item{time}{A vector of event times in \code{\link{block-format}} with respect to \code{id}.}
    \item{ii}{A vector of interdose intervals in
        \code{\link{block-format}} with respect to \code{id}, as
        returned by \code{blk.addl}.}
    \item{id}{A valid \code{\link{block-format}} ID.}
    \item{dose.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
        Indicates rows corresponding to dose events.}
    \item{dose}{Vector of dose amounts in \code{\link{block-format}} with respect to \code{id}.}
    \item{tol.ii}{A numeric tolerance for comparing the interdose intervals.  See details.}
    \item{tol.dose}{A numeric tolerance for comparing dose amounts. See details.}
    \item{min.consec}{An integer specifying the minumum number of dose
        records to be considered for \code{ADDL} grouping.}
    \item{include.last}{A logical.  Should the last dose before a
        non-dosing event be considered for merging?}
    \item{diff.op}{A function that subtracts one time from another.}
}
\details{

    These functions operates on a PK data set in \code{\link{block-format}}.
    Event times are specified by \code{time} and rows that corresponding to dose events are
    indicated by \code{dose.ind}.

    In NONMEM (many other software packages used in pharmcometrics
    have adopted the same convention), multiple dose events with the
    same dose amount given at a fixed time interval can be specified
    on a single dose record by specifying two variables: \code{ADDL}
    for the number of \emph{additional doses} (i.e., not counting the
    current dose record, making the total number of dose events equal
    to \code{ADDL + 1}), and \code{II} for the \emph{interdose
    interval} (the fixed time interval between consecutive dose
    events).  These functions will help to derive these variable from
    a data set in which each dose event is its own records, leading to
    a more compact dataset (in some cases, much more compact if many
    dose records can be eliminated).

    When determining if multiple dose records can safely be combined,
    successive dose amounts and iterdose time intervals need to be
    compared to make sure they are the same. Those numeric comparisons
    can be made to have a certain tolerance, specified by the
    \code{tol.dose} and \code{tol.ii} respectively. If two successive
    values are within the tolerance of each other in absolute value,
    they will be considered the same.

}
\value{
    For \code{blk.ii}, a numeric vector in \code{\link{block-format}} with respect to \code{id}
    containing the interdose intervals.

    For \code{blk.addl}, a numeric vector in \code{\link{block-format}} with respect to \code{id}
    containing the \code{ADDL} count for dose events that could represent
    multiple doses, the value -1 for dose events that should be removed if
    \code{ADDL} dosing records are used, and 0 for all other events.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
    \itemize{
    \item \code{\link{block-format}}
    \item \code{\link{blk.intereventTime}}
    \item \code{\link{blk.isSteadyState}}
    \item \code{\link{blk.findConsecutive}}
    \item \code{\link{difftime.default}}
    }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, II=blk.ii(time, asID(Subject), !is.na(dose)),
           ADDL1=blk.addl(blk.ii(time, asID(Subject), !is.na(dose)), asID(Subject), !is.na(dose), dose),
           ADDL2=blk.addl(blk.ii(time, asID(Subject), !is.na(dose)), asID(Subject), !is.na(dose), dose, min.consec=5),
           ADDL3=blk.addl(blk.ii(time, asID(Subject), !is.na(dose)), asID(Subject), !is.na(dose), dose, include.last=TRUE))

detach(dat)

}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
