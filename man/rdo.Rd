% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvar-rfun.R
\name{rdo}
\alias{rdo}
\title{Execute expressions of random variables}
\usage{
rdo(expr, dim = NULL, ndraws = NULL)
}
\arguments{
\item{expr}{(expression) A bare expression that can (optionally) contain
\code{\link{rvar}}s. The expression supports \link[rlang:topic-inject]{quasiquotation}.}

\item{dim}{(integer vector) One or more integers giving the maximal indices
in each dimension to override the dimensions of the \code{\link{rvar}} to be created
(see \code{\link[=dim]{dim()}}). If \code{NULL} (the default), \code{dim} is determined by the input.
\strong{NOTE:} This argument controls the dimensions of the \code{\link{rvar}}, not the
underlying array, so you cannot change the number of draws using this
argument.}

\item{ndraws}{(positive integer) The number of draws used to construct new
random variables if no \code{\link{rvar}}s are supplied in \code{expr}. If \code{NULL},
\code{getOption("posterior.rvar_ndraws")} is used (default 4000). If \code{expr}
contains \code{\link{rvar}}s, the number of draws in the provided \code{\link{rvar}}s is used
instead of the value of this argument.}
}
\value{
An \code{\link{rvar}}.
}
\description{
Execute (nearly) arbitrary \R expressions that may include \code{\link{rvar}}s,
producing a new \code{\link{rvar}}.
}
\details{
This function evaluates \code{expr} possibly multiple times, once for each draw of
the \code{\link{rvar}}s it contains, then returns a new \code{\link{rvar}} representing the output of those
expressions. To identify \code{\link{rvar}}s, \code{rdo()} searches the calling environment for any variables
named in \code{expr} for which \code{\link[=is_rvar]{is_rvar()}} evaluates to \code{TRUE}. If \code{expr} contains no \code{\link{rvar}}s,
then it will be executed \code{ndraws} times and an \code{\link{rvar}} with that many draws returned.

\code{rdo()} is not necessarily \emph{fast} (in fact in some cases it may be very slow), but
it has the advantage of allowing a nearly arbitrary R expression to be executed against \code{\link{rvar}}s
simply by wrapping it with \code{rdo( ... )}. This makes it especially useful as a prototyping
tool. If you create code with \code{rdo()} and it is unacceptably slow for your application,
consider rewriting it using math operations directly on \code{\link{rvar}}s (which should be fast),
using \code{\link[=rvar_rng]{rvar_rng()}}, and/or using operations directly on the arrays that back the \code{\link{rvar}}s
(via \code{\link[=draws_of]{draws_of()}}).
}
\examples{

mu <- rdo(rnorm(10, mean = 1:10, sd = 1))
sigma <- rdo(rgamma(1, shape = 1, rate = 1))
x <- rdo(rnorm(10, mu, sigma))
x

}
\seealso{
Other rfun: 
\code{\link{rfun}()},
\code{\link{rvar_rng}()}
}
\concept{rfun}
