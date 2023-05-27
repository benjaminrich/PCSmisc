\name{crcl.cg}
\alias{crcl.cg}
\title{Creatinine Clearance Using the Cockroft-Gault Formula}
\description{
  Computes estimated creatinine clearance using the Cockroft-Gault
  formula.
}
\usage{
crcl.cg(..., scr.mg.dL, weight.kg, age.yr, is.female, scr.umol.L, scr.conversion.factor = 88.4)
}
\arguments{
  \item{...}{Ignored. This forces all parameters to be named, which is safer.}
  \item{scr.mg.dL}{Numeric. Serum creatinine in mg/dL.}
  \item{weight.kg}{Numeric. Body weight in kilograms.}
  \item{age.yr}{Numeric. Age in years.}
  \item{is.female}{Logical. TRUE for females, FALSE for males.}
  \item{scr.umol.L}{Numeric. Alternative specification of serum creatinine in umol/L.}
  \item{scr.conversion.factor}{Numeric. A factor for converting serum creatinine from umol/L to mg/dL.}
}
\details{
  All arguments can be vectors.
}
\references{
  \url{http://en.wikipedia.org/wiki/Creatinine_clearance}
}
\value{
  A numeric vector containing the estimated creatinine clearance in mL/min.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{egfr.mdrd}}
}
\examples{

# With serum creatinine in mg/dL
crcl.cg(scr.mg.dL=0.9, weight.kg=112.9, age.yr=53, is.female=FALSE)

# With serum creatinine in umol/L
crcl.cg(scr.umol.L=0.9*88.4, weight.kg=112.9, age.yr=53, is.female=FALSE)

}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
