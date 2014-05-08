\name{CommonThreadDescriptions}
\alias{CommonThreadDescriptions}
\title{Generate plain text descriptions of the CommonThreads}
\usage{
CommonThreadDescriptions(X)
}
\arguments{
  \item{X}{output of CommonThreads}
}
\value{
character vector with plain text desctions for each Thread
in decreasing order of effect.
}
\description{
Uses the output of the CommonThreads function and describes
those common theads in plain text.
}
\examples{
data(iris)
observations.selected <- sample(1:nrow(iris), 30)
result.ct <- CommonThreads(X=iris, members=observations.selected)
CommonThreadDescriptions(result.ct)$prose
}
\author{
Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\seealso{
\code{\link{CommonThreads}}
}

