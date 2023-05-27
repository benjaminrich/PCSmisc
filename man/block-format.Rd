\name{block-format}
\alias{block-format}
\title{Block Format}
\description{
Block format is the typical `long' format for longitudinal data, whereby the
data for an individual is represented by a block of rows in a data set, and an
ID column (or in some cases multiple columns) serves to delineate the blocks.
It is also called `stacked' data format.

For the purposes of the \code{PCSmisc} library, a block format is defined
through an ID, which \emph{must} be a \code{\link{factor}} and \emph{must}
possess the property of being sorted: i.e.  for a variable \code{id} to be a
valid ID, the following expression must evaluate to \code{TRUE}:
\code{\link{is.factor}(id) && !\link{is.unsorted}(id)}.

This strict requirement is intended to reduce the likelihood of committing
errors when using functions in \code{PCSmisc} that operate on data in block
format.  In case the column(s) that define the blocks in a particular data set
do not conform to these requirements, a valid ID can easily be constructed
using \code{\link{asID}}.

The size of a block is the number of times the corresponding ID appears in
the data.  A block can also have size zero.  This occurs when there are levels
of the ID that do not appear in the data at all.  The size of each block can
most easily be obtained using \code{\link{blk.count}}.

\code{PCSmisc} contains a number of functions that operate on data in block
format.  These functions are identifiable by the prefix \code{blk.} and are
generally wrappers of \code{\link{tapply}}.  Like \code{\link{tapply}}, these
functions do not operate on \code{\link{data.frame}}'s directly but rather on
their columns (i.e. on individual vectors of the same length).  The purpose of
these functions is to provide abstractions for common data operations,
resulting in code that is easier to understand and reducing the likelihood of
errors.  Some of these functions are quite general, others are more specific to
the creation of population PK type datasets, as required e.g. for analysis with
NONMEM.  See examples below.
}

\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{asID}}
  \code{\link{blk.count}}
  \code{\link{blk.repeatValue}}
  \code{\link{tapply}}
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects

attach(dat)

is.sorted(Subject)          # FALSE
is.sorted(asID(Subject))    # TRUE

data.frame(id=asID(Subject),
           time=time,
           dose=dose,
           first.dose=blk.repeatValue(dose, asID(Subject), ind=(!is.na(dose))),
           conc=conc,
           count.conc=blk.count(asID(Subject), ind=!is.na(conc)),
           last.dose=blk.locf(dose, asID(Subject)),
           time.after.dose=blk.tad(time, asID(Subject), !is.na(dose)))

detach(dat)

### The size of each block (example)
id <- factor(c("A", "A", "C", "C", "C"), levels=c("A", "B", "C"))
blk.count(id, NULL)  # Note that "B" is a valid ID.  The corresponding block has size zero.

}
\keyword{ utilities }
