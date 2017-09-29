\name{pkstudy.CM}
\alias{pkstudy.CM}
\docType{data}
\title{
Concomitant Medication Data from an Imaginary PK Study
}
\description{
This \code{data.frame} contains data on concomitant medication from an imaginary PK study.
}
\usage{data(pkstudy.CM)}
\format{
  A data frame with 113 observations on the following 6 variables.
  \describe{
    \item{\code{studyid}}{a factor with levels \code{XXX-YYY-ZZZ}}
    \item{\code{domain}}{a factor with levels \code{CM}}
    \item{\code{subjid}}{a factor with levels \code{001-001} \code{001-002} \code{001-003} \code{001-004} \code{001-005} \code{001-006} \code{002-001} \code{002-003} \code{002-004} \code{002-006}}
    \item{\code{cmtext}}{a factor.  The names of theo concomitant medication.}
    \item{\code{cmstdtc}}{a factor.  The date the concomitant medication was started in \code{yyyy-mm-dd} format.}
    \item{\code{cmendtc}}{a factor.  The date the concomitant medication was ended in \code{yyyy-mm-dd} format.}
  }
}
\details{
This data is part of a toy database from an imaginary PK study.  The
data is meant to resemble a standard CDISC format.  Briefly,
\code{pkstudy.DM} contains demographic data, \code{pkstudy.PC}
contains pharmacokinetic concentrations, \code{pkstudy.EX} contains
exposure data, \code{pkstudy.VS} contains vitals signs,
\code{pkstudy.LB} contains laboratory tests, and \code{pkstudy.CM}
contains data on concomitant medications.  All the data are linked by
a \code{subjid} field, the ID that identifies the individual subjects
in the study.
}
\seealso{
pkstudy.PC
pkstudy.DM
pkstudy.EX
pkstudy.VS
pkstudy.LB
}
\examples{
data(pkstudy.CM)
}
\keyword{datasets}

% vim: tw=70 sw=2
