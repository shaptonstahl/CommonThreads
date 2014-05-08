\name{CommonThreads}
\alias{CommonThreads}
\title{Which variables best explain membership?}
\usage{
CommonThreads(X, members, min.nonzero.certainty = 0.75,
  classifier = c("probit", "forest"))
}
\arguments{
  \item{X}{data.frame of observations}

  \item{members}{numeric vector of rows included or logical
  vector with TRUE for 'members' (included rows)}

  \item{min.nonzero.certainty}{numeric p-value must be
  below 1-min.nonzero.certainty to report result for a
  coefficient}

  \item{classifier}{character singleton specifying which of
  \code{probit} or \code{forest} will be used}
}
\value{
list containing the following: \tabular{lll}{
\code{threads} \tab \preformatted{ } \tab A
\code{data.frame} with columns \code{column.name},
\code{effect}, and \code{prob.not.zero}; see
\emph{Details}\cr \code{X} \tab \tab as passed to
\code{CommonThreads}\cr \code{members} \tab \tab as passed
to \code{CommonThreads}\cr \code{min.nonzero.certainty}
\tab \tab as passed to \code{CommonThreads} }
}
\description{
Identify which variables explain best why these
observations/nodes are distinct from those observations.
}
\details{
\code{CommonThreads} uses a binary classifier (probit
regression for now; random forest classifier forthcoming)
with membership in \code{members} as the dependent variable
to identify which variables are best at predicting
membership.

A column \code{y} of 1s and 0s is created, length equal to
the number of rows of \code{X}, with 1 for each row that is
a member and 0 for each other row.  This is the dependent
variable of the classifier. The columns of \code{X} are the
independent variables. Then the classifier is run and the
more influential variables are identified.

The returned data.frame \code{threads} has three columns.
The first, \code{column.name}, comes from the name of the
column of \code{X}. If a column of \code{X} was a
\code{factor} or coerced into one then \code{column.name}
will be the name of the column of \code{X}, a period, and
the value of the factor variable that was identified to be
influential.  The second column gives the size of the
effect for each variable but it is recommended that this be
used only for comparison with other variables.

The third column, \code{prob.not.zero}, actually gives one
minus the p-value associated with the coefficient.
Frequentist purists will argue that this is not the
probability of the alternative (non-zero) hypothesis being
true; purist Bayesians will argue that this is not a proper
posterior probability of anything. A thoughtful pragmatist
will note that this is a good frequentist estimate for the
Bayesian posterior probability that the coefficient does
not have the wrong sign, assuming a flat (uninformative)
prior and that there is a sufficient amount of data to have
reached Asymptopia. Use with care, if at all.

\subsection{Probit Classifier}{ Before the probit
regression is run, each variable in \code{X} is
standardized by subtracting the column mean and dividing by
the column standard deviation. This forces the units of the
resulting coefficients to be \dQuote{change in latent
variable per standard deviation of x}. For linear
regression this would have the same effect as standardizing
the coefficients in that the coefficients would be
comparable directly with each other to see which has the
greatest effect. Here with probit regression it makes the
size of the regression coefficient larger precisely when a
change in a variable from one standard deviation below the
mean to one above the mean has the larger change in the
predicted probability of being a member.

The returned \code{effect} is not intended to be used
directly, but can be useful in comparing the size of
effects.  For example, if V1 has an effect of 2 and V1 has
an effect of 1, V1 is \dQuote{twice as influential} in
predicting membership, though what that means exactly is
more subtle. }

\subsection{Random Forest Classifier}{ \emph{not
implemented; forthcoming} }

\subsection{Factor Variables}{ If a column of \code{X} is a
\code{factor} or can be coerced into one by
\code{dummy.data.frame(dummies)} (say, because it is a
character variable) then it will be broken into a set of
dummy variables, one for each value of the column.  Each of
these dummy variables is tested for its influence over
membership but combinations of values are not considered. }
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

