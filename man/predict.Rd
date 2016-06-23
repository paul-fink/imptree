\name{predict}
\alias{predict}
\alias{predict.imptree}
\alias{predict.impbag}

\title{Classification with Imprecise Probabilities}
\description{
  \code{predict.imptree} and \code{predict.impbag} perform the prediction of \code{imptree} and \code{impbag} objects
}

\usage{
  \method{predict}{imptree}(object, newdata = NULL, type = c("class", "prob"), 
    dominance = c("strong", "max"), \dots)
  \method{predict}{impbag}(object, newdata = NULL, dominance = c("strong", "max"),
    aggregation = c("no", "equal", "dacc", "disjunction", "mean"),  \dots)
}

\arguments{
  \item{object}{an object of class \code{\link{imptree}} or \code{\link{impbag}}}
  \item{newdata}{\code{data.frame} containing observations to be predicted. See details.If \code{NULL} the observations 
    in the training set of \code{object} are employed. Any observation with a \code{NA} value
    in any splitting variable will be omitted}
  \item{type}{Type of prediction: This may be either \code{class} (Default) for class
    or \code{prob} for probability interval prediction.}
  \item{dominance}{Dominance criterion to apply when predicting classes.  See \code{\link{predclass}} for details.}
  \item{aggregation}{Aggregation method to be applied. See details.}
  \item{\dots}{optional parameters, unused at the moment.}
}

\value{
  For an \code{\link{imptree}} object the result is a list with a single entry \code{predicted} which contains for
  \item{type = "class"}{a boolean \code{data.frame}, where each row is an observation and each column a class. 
    \code{TRUE} is indicating that the class is predicted.}
  \item{type = "prob"}{a list with length of the number of observations. 
    Each entry is a named list giving giving the upper (\code{Upper}) and lower (\code{Lower}) 
    bounds of the probability for the classes.}   
  For an \code{\link{impbag}} object the result is a named list with the following entries:
  \item{predicted}{Object representating the predicted classes; This is a list for \code{aggregation = "no"},
    with each entry a tree in the bag, else a \code{data.frame} with same properties as for an \code{imptree} object}
  \item{probs}{Object representating the predicted class probability intervals; 
    \code{NULL} for \code{aggregation = "equal"} and \code{aggregation = "dacc"}, a list with each entry a tree for \code{aggregation = "no"}
    and else a single entry of same type as the result for an \code{imptree} object with \code{type = "prob"}.}
}

\details{
  As for predicting imprecise classification trees one may base them on the classes
  in the leaves or the achieved probability intervals. With \code{type = "class"} the first
  approach is employed while \code{type = "prob"} uses the latter.

  When predicting impags both the prediction for each tree in the bag and the one of the aggregate may be obtained:
  For class based aggregates one can choose between
  \itemize{
  \item{aggregation = "no"}{no aggregation (default)} 
  \item{aggregation = "equal"}{majority voting}
  \item{aggregation = "dacc"}{majority voting on weighted trees according to their achived discounted-accuracy}
  }

  For probability interval based aggregates one may choose between
  \itemize{
  \item{aggregation = "no"}{no aggregation (default)}
  \item{aggregation = "disjunction"}{disjunction rule and}
  \item{aaggregation = "mean"}{average rule}
  }
  
}

\author{Paul Fink \email{Paul.Fink@stat.uni-muenchen.de}, based on an algorithm
    employed for prediciton in package \pkg{TWIX}.
}

\seealso{\code{\link{imptree}}, \code{\link{impbag}}, \code{\link{accuracy}}, \code{\link{predclass}}}

\examples{
  data(car_acceptance)
  # a subset of the data set
  car_sub <- car_acceptance[sample(1:NROW(car_acceptance), 100), ] 
  
  # growing a bag of 10 trees
  bag <- impbag(acceptance ~. , data = car_sub,
           depth = 2, boot = 10)
  
  # difference in dominace method for a single tree
  predict(bag$Greedy.tree, type = "class", dominance = "max")
  predict(bag$Greedy.tree, type = "class")

  # predicting the whole bag 
  \dontrun{
  # with majority voting
  predict(bag, aggregation = "equal", dominance = "max")
  # with disjunction rule
  predict(bag, aggregation = "disjunction", dominance = "max")
  }
  # using different data to predict on a single tree
  train_data <- car_sub[trainindex <- sample(
    1:NROW(car_sub), replace = FALSE, size = 40), ]
  test_data  <- car_sub[setdiff(
    1:NROW(car_sub), trainindex), ]
  tree <- imptree(acceptance ~., data = train_data, depth = 2)
  # predicting with the test_data
  predict(tree, newdata = test_data)
}

\keyword{tree}
