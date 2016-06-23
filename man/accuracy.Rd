\name{accuracy}
\alias{accuracy}

\title{Classification with Imprecise Probabilities}
\description{
  \code{accuracy} assesses the accuracy obtained by 
  \code{imptree} and \code{impbag} objects.
}

\usage{
  accuracy(object, data = NULL, aggmethod = c("equal", "dacc",
    "disjunction", "mean"), dominance = c("strong", "max"))
}

\arguments{
  \item{object}{an object of class \code{\link{imptree}} or
    \code{\link{impbag}}}
  \item{data}{data to be used as base of the accuracy
    assessment. See details}
  \item{aggmethod}{Aggregation method, ignored for 
    \code{\link{imptree}} object. For an overview of all
    available types, see details. The value of the argument
    can be abbreviated}
  \item{dominance}{Dominance criterion to apply when
     predicting classes. See \code{\link{predclass}} for
     details.}
}

\value{
  A named list containing the following components
  \item{acc}{A list with the values for the obtained 
    \itemize{
    \item{\code{determinacy}}{: Percentage of observations 
      predicted as a single class,}
    \item{\code{singleacc}}{: Correct classification rate
      based on determinate observations,}
    \item{\code{setacc}}{: Correct classification rate
      based on indeterminate observations,}
    \item{\code{nindeterminant}}{: Average number of
      classes obtained when predicting indeterminate and} 
    \item{\code{discountedacc}}{: Discounted accuracy,
      correct classification rate wrighted by the number of
       predicted classes).}}}
  \item{prediction}{object returned by call to
    \code{\link{predict}}.}
}

\details{
  To assess the accuracy a prediciton of the \code{object}
  is performed before. If \code{data} is \code{NULL} 
  or not supplied, then the data of \code{object} are used
  for the prediction, i.e the training data.
  
  To obtain class predicitions for \code{\link{impbag}} an
  aggregation method needs to be supplied to the prediction.
  Available aggregation methods are:
  \itemize{
  \item{\code{equal}}{: majority voting}
  \item{\code{dacc}}{: majority voting weighted by the
    discounted accuracy}
  \item{\code{disjunction}}{: disjunction rule}
  \item{\code{mean}}{: mean rule}
  }
}


\author{
  Paul Fink \email{Paul.Fink@stat.uni-muenchen.de}.
}

\references{
  Zaffalon, M. and Corani, G. and \ifelse{latex}{\out{Mau\'{a}}}{\ifelse{html}{\out{Mau&aacute;}}{Maua}}, D. (2011), Utility-Based Accuracy Measures to Empirically Evaluate Credal Classifiers, \emph{ISIPTA'11: Proceedings of the Seventh International Symposium on Imprecise Probability: Theories and Applications}, 401--410 
}

\seealso{\code{\link{imptree}}, \code{\link{impbag}}, 
    \code{\link{predict.imptree}}, \code{\link{predclass}}}

\examples{
  data(car_acceptance)
  
  # subset of the data set
  car_sub <- car_acceptance[sample(1:NROW(car_acceptance), 100), ]

  # growing a single imprecise tree  
  tree <- imptree(acceptance~., data = car_sub, depth = 2)
  
  acc <- accuracy(tree)
  str(acc)
  # extracting the accuracy measures
  acc$acc
}


\keyword{tree}
