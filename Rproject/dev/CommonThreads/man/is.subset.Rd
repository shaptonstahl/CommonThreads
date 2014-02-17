\name{is.subset}
\alias{is.subset}
\title{Checks to see if the elements of one vector/array are contained
in another vector/array}
\usage{
is.subset(x, y)
}
\arguments{
  \item{x}{vector or coercible to one}

  \item{y}{vector or coercible to one}
}
\value{
logical singleton
}
\description{
This is an aggregation of the "%in%" function.
}
\examples{
is.subset(1:3, 1:5)  # returns TRUE
is.subset(1:5, 1:3)  # returns FALSE
is.subset(array(1:9, dim=c(3,3)), c(1:100))  # returns TRUE
is.subset(matrix(1:200, ncol=2), array(1:125, dim=c(5,5,5)))  # returns FALSE
}
\author{
Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\seealso{
\code{\link{CommonThreads}}
}

