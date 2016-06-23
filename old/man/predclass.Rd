\name{predclass}
\alias{predclass}

\title{Classification with Imprecise Probabilities}

\description{
  \code{predclass} performs actual predicting of classes given the
  probability intervals for each class
}

\usage{
  predclass(lower, upper, method = c("strong", "max"))
}

\arguments{
  \item{lower}{named numeric array with each entry the lower bound of the probability interval for the associated class state}
  \item{upper}{named numeric array with each entry the upper bound of the probability interval for the associated class state}
  \item{method}{dominance criterion to be applied when deciding for undominated classes}
}

\value{
  named boolean array, with each entry indicating if the class is undominated according to the dominace criterion
}

\details{
  There are currently 2 different dominance criteria available.
  \itemize{
    \item{\code{max}}{: Maximum frequency criterion. Dominance is decided only by the upper bound of the probability interval, ie.
      a state \eqn{C_i} is dominated if there exists any \eqn{j \neq i}{j != i} with \eqn{u(C_i) < u(C_j)}
    }
    \item{\code{strong}}{: Interval dominance criterion. For the IDM it coincides with the strong dominance criterion. Here
      a state \eqn{C_i} is dominated if there exists any \eqn{j \neq i}{j != i} with \eqn{u(C_i) < l(C_j)}
    }
  }
}

\author{
  Paul Fink \email{Paul.Fink@stat.uni-muenchen.de}.
}

\seealso{\code{\link{imptree}}, \code{\link{impbag}}, 
    \code{\link{predict.imptree}}, \code{\link{accuracy}}}

\examples{
# For a classification variable with 4 states and the frequencies
# (1, 0, 3, 4) the IDM (s = 2) yields the following probability
# interval bounds
lower <- c(A = 0.1, B = 0, C = 0.3, D = 0.4)
upper <- c(A = 0.3, B = 0.2, C = 0.5, D = 0.6)

p1 <- predclass(lower, upper)                   
p1                      # c(FALSE, FALSE, FALSE, TRUE)
p2 <- predclass(lower, upper, method = "strong")
p2                      # c(FALSE, FALSE, TRUE, TRUE)
\dontshow{isTRUE(all(p1==predclass(lower, upper, m = "m")))}
}

\keyword{tree}
