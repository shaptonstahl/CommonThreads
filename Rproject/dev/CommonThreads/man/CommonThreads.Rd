\name{CommonThreads}
\alias{CommonThreads}
\title{Which variables best explain membership?}
\usage{
CommonThreads(X, members, min.nonzero.certainty = 0.75)
}
\arguments{
  \item{X}{data.frame of observations}

  \item{members}{numeric vector of rows included or logical
  vector with TRUE for members}

  \item{min.nonzero.certainty}{numeric p-value must be
  below 1-min.nonzero.certainty to report result for a
  coefficient}
}
\value{
data.frame with variables column.name, effect,
prob.not.zero
}
\description{
Identify which variables explain best why these
observations/nodes are distinct from those observations.
}
\examples{
data(iris)
observations.selected <- sample(1:nrow(iris), 30)
CommonThreads(X=iris, members=observations.selected)
CommonThreads(X=iris, members=observations.selected, min.nonzero.certainty=.5)
}
\author{
Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\seealso{
Use the output to describe the relationships in plain text
with \code{\link{CommonThreadDescriptions}}.
}

