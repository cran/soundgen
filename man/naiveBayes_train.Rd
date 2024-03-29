% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naiveBayes.R
\name{naiveBayes_train}
\alias{naiveBayes_train}
\title{Train a naive Bayes classifier}
\usage{
naiveBayes_train(formula, data)
}
\arguments{
\item{formula}{outcome ~ predictor1 + predictor1 + ...}

\item{data}{training dataset}
}
\description{
Returns conditional means and standard deviations per class as well as a
table with the global proportions of each class in the dataset. This is
mostly useful because the output can be passed on to \code{\link{naiveBayes}}
to save time if naiveBayes() is called in a loop with the same training
dataset.
}
