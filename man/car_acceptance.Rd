\name{car_acceptance}
\alias{car_acceptance}
\docType{data}
\title{
  Car Evaluation Database  
}

\description{
  This is the 'Car Evaluation' data set from the UCI Machine 
  Learning Repository.

  The Car Evaluation Database gives the acceptance of a car
  directly related to the six input attributes: buying, 
  maint, doors, persons, lug_boot, safety.
}

\usage{data(car_acceptance)}

\format{
  A data frame with 1728 observations on the following 7 variables.
  \describe{
    \item{\code{buying}}{buying price of the car as a factor with levels \code{high} \code{low} \code{med} \code{vhigh}}
    \item{\code{maint}}{price of the maintenance as a factor with levels \code{high} \code{low} \code{med} \code{vhigh}}
    \item{\code{doors}}{number of doors as a factor with levels \code{2} \code{3} \code{4} \code{5more}}
    \item{\code{persons}}{capacity in terms of persons to carry as a factor with levels \code{2} \code{4} \code{more}}
    \item{\code{lug_boot}}{the size of luggage boot as a factor with levels \code{big} \code{med} \code{small}}
    \item{\code{safety}}{estimated safety of the car as a factor with levels \code{high} \code{low} \code{med}}
    \item{\code{acceptance}}{acceptance of the car (target variable) as a factor with levels \code{acc} \code{good} \code{unacc} \code{vgood}}
  }
}

\details{
  Car Evaluation Database was derived from a simple hierarchical
  decision model originally developed for the demonstration of DEX.
  The model evaluates cars according to the following concept structure:

  CAR                      car acceptability

  . PRICE                  overall price

  . . buying               buying price

  . . maint                price of the maintenance

  . TECH                   technical characteristics

  . . COMFORT              comfort

  . . . doors              number of doors

  . . . persons            capacity in terms of persons to carry

  . . . lug_boot           the size of luggage boot

  . . safety               estimated safety of the car

  Input attributes are printed in lowercase. Besides the target
  concept (CAR), the model includes three intermediate concepts:
  PRICE, TECH, COMFORT. 

  The Car Evaluation Database contains examples with the structural
  information removed, i.e., directly relates CAR to the six input
  attributes: buying, maint, doors, persons, lug_boot, safety.
}

\source{
  The hierarchical decision model, from which this dataset is
  derived, was first presented in 

  M. Bohanec and V. Rajkovic (1988), Knowledge acquisition and explanation for
  multi-attribute decision making, \emph{8th Intl. Workshop on Expert
  Systems and their Applications}, Avignon, France, 59--78.

  The original data is at \url{http://archive.ics.uci.edu/ml/datasets/Car+Evaluation}.
}

\seealso{
  \code{\link{imptree}}
}

\examples{
  data(car_acceptance)
  summary(car_acceptance)

  stopifnot(require(imptree))

  # limiting the depth of the tree
  acceptTree <- imptree(acceptance ~ ., data = car_acceptance, depth = 3)
  print(acceptTree)
  accuracy(acceptTree)$acc
}

\keyword{datasets}
