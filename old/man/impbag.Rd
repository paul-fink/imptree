\name{impbag}
\alias{impbag}
\alias{impbag.formula}
\alias{impbag.default}
\alias{print.impbag}

\title{Classification with Imprecise Probabilities}
\description{
  \code{impbag} generates bagging of \code{imptree}. 
    Accuracy of a test set can be assessed by various
    aggregation methods
}

\usage{
\method{impbag}{default}(x, y, \dots)
\method{impbag}{formula}(formula, data = NULL, testdata = NULL,
  na.action = na.pass, control, gamma = 1, Tbase = 1, 
  method = c("IDM", "NPI"), method.param,
  aggregation = c("equal", "dacc", "disjunction", "mean"), \dots)
\method{print}{impbag}(x, \dots)
}

\arguments{
  \item{data}{an optional data frame containing the
    variables in the model.By default the variables are
    taken from the environment which \code{imptree} is
       called from.}
  \item{x, formula}{a data frame or a matrix of features,
    or a formula describing the model to be fitted (for the
    \code{print} method, an \code{imptree} object).}
  \item{y}{A response vector, being a factor.}
  \item{testdata}{A \code{data.frame} for the test set.
    (NOTE: If given it needs to contain both the class and
    features)}
  \item{na.action}{A function to specify the action to be
    taken if \code{NA}s are found. (Default: na.pass)
    (NOTE: If given, this argument must be named.)}
  \item{control}{a named list according the result of \code{\link{imptree_control}}.
    May be a subset of the list}
  \item{gamma}{The weighting factor of the pessimism when
    calculating the t-value. Setting to 1 (Default) is weakest restriction}
  \item{Tbase}{The minimum t-value that needs to be attained to qualify for a split.}
  \item{method}{Method applied for calculating the probability intervals of the
    class probability. \code{"IDM"} for the Imprecise Dirichlet Model (Default) and
    \code{"NPI"} for use of the Nonparametric Predictive Inference approach.}
  \item{method.param}{Named list specifying the mehtod specific parameter. 
    See details at \code{\link{imptree}}}
  \item{aggregation}{Aggregation method to be applied to
    combine the trees within the bag. See details. The value
    of the argument can be abbreviated}
  \item{...}{optional parameters to be passed to the main
    function \code{impbag.formula} or to the call of \code{\link{imptree_control}}.}
}

\value{
  An object of class \code{impbag}, which is a list with the
  following components:
  \item{Call}{the original call to \code{impbag}}
  \item{Bag}{A list containing the tree structure for each
    bootstrap sample}
  \item{Greedy.tree}{A list containing the tree structure
    based on all data.}
  \item{OoBAcc}{The accuracy based on the out-of-bag
    instances. \code{NULL} if \code{calc.oobe = FALSE}.}
  \item{Accuracy}{Accuracy achieved on training data.
    \code{NULL} if \code{calc.bag.acc = FALSE}.}
  \item{BIndices}{List containing the indices of the data
    employed in each sampling}
  \item{Data}{The training data used to construct the tree.}
  \item{formula}{The formula describing the data structure, 
    which was used in fitting.}
  \item{Test}{Accuracy achieved on supplied test data.
    \code{NULL} if \code{testdata = NULL}.}
}

\details{
  There are 3 main different aggregation rules: Majoirty
  voting, Disjunction and Mean rule.
  
  For the majority voting an equal weighting (\code{equal})
  and a weighting, based on the discounted-accuracy of each
  tree (\code{dacc}), may be applied on the bag.

  The disjunction rule is based on the predicted
  probabilities and forms the union over all trees in the
  bag. It is extremely cautious.

  Also based on predicted probabilities is the Mean rule. It
  takes the mean over all lower and upper bounds of the
  probability intervals for an observation for each class.

  To obtain the classes for both the Mean and the Disjunction
  rule the dominance criterion supplied within the \code{control}
  arguement is applied, see \code{\link{imptree_control}}

  If no aggregation rule is specified, an unweighted
  majority voting is carried out.
}

\references{
  \ifelse{latex}{\out{Abell\'{a}n, J. and Moral, S.}}{
  \ifelse{html}{\out{Abell&aacute;n, J. and Moral, S.}
  }{Abellan, J. and Moral, S.}} (2005), \dQuote{Upper entropy
  of credal sets. Applications to credal classification},
  International Journal of Approximate Reasoning 39, 235--255.
}

\author{Paul Fink \email{Paul.Fink@stat.uni-muenchen.de}
}

\seealso{\code{\link{predict.impbag}},
  \code{\link{accuracy}}, \code{\link{predclass}}, \code{\link{imptree_control}}}

\examples{
  data(car_acceptance)
  
  # a subset of the complete data set
  car_sub <- car_acceptance[sample(1:NROW(car_acceptance), 100), ] 

 
  bag <- impbag(acceptance ~ ., data = car_sub, 
    control = list(boot = 10, calc.oobe = TRUE),
    calc.bag.acc = TRUE, depth = 2)

  # comparing the accuracy
  accuracy(bag)$acc
  bag$Accuracy$acc
  bag$OoBAcc
  
  # simple prediction
  pred <- predict(bag, aggregation = "mean")
  pred$predicted
}

\keyword{tree}
