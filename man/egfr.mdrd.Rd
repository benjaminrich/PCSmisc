\name{egfr.mdrd}
\alias{egfr.mdrd}
\title{Estimated GFR Using the MDRD Formula}
\description{
  Computes estimated glomerular filtration rate (GFR) using the
  Modification of Diet in Renal Disease (MDRD) formula.
}
\usage{
egfr.mdrd(..., scr.mg.dL, age.yr, is.female, is.black, bun.mg.dL=NULL, albumin.g.dL=NULL, scr.umol.L, scr.conversion.factor = 88.4)
}
\arguments{
  \item{...}{Ignored. This forces all parameters to be named, which is safer.}
  \item{scr.mg.dL}{Numeric. Serum creatinine in mg/dL.}
  \item{age.yr}{Numeric. Age in years.}
  \item{is.female}{Logical. TRUE for females, FALSE for males.}
  \item{is.black}{Logical. TRUE if race is 'black', FALSE otherwise.}
  \item{bun.mg.dL}{Numeric. Blood urea nitrogen (BUN) concentrations in mg/dL (optional).}
  \item{albumin.g.dL}{Numeric. Albumin concentration in g/dL (optional).}
  \item{scr.umol.L}{Numeric. Alternative specification of serum creatinine in umol/L.}
  \item{scr.conversion.factor}{Numeric. A factor for converting serum creatinine from umol/L to mg/dL.}
}
\details{
  All arguments can be vectors.
  Albumin and BUN are optional.  If they are specified, a more elaborate version
  of the MDRD equation is used.
}
\value{
  A numeric vector containing the estimated GFR mL/min.
}
\references{
    \url{http://en.wikipedia.org/wiki/Creatinine_clearance}
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{crcl.cg}}
}
\examples{

# With serum creatinine in mg/dL
egfr.mdrd(scr.mg.dL=0.9, age.yr=53, is.female=FALSE, is.black=FALSE)

# With serum creatinine in umol/L
egfr.mdrd(scr.umol.L=0.9*88.4, age.yr=53, is.female=FALSE, is.black=FALSE)

}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
