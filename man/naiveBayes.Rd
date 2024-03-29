% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naiveBayes.R
\name{naiveBayes}
\alias{naiveBayes}
\title{Naive Bayes}
\usage{
naiveBayes(
  formula,
  train,
  test = train,
  prior = c("flat", "static", "dynamic")[2],
  wlPrior = 3,
  wlClumper = NULL,
  runBack = TRUE,
  plot = FALSE
)
}
\arguments{
\item{formula}{model formula of the type outcome ~ predictor1 + predictor2 +
... (no interactions)}

\item{train}{either the training dataframe or the output of
\code{\link{naiveBayes_train}}. This data is used to calculate
class-specific distributions of the predictors and prior class
probabilities}

\item{test}{the test dataframe. This data is used to make predictions - that
is, outcome class probabilities given the values of predictors}

\item{prior}{"flat" = all classes are equally likely a prior, "static" = use
class probabilities in the training dataset, "dynamic" = update prior
probabilities from weighted likelihoods of \code{wlPrior} preceding
observations}

\item{wlPrior}{the length of a Gaussian window used for updating dynamic
priors}

\item{wlClumper}{the minimum expected number of observations of the same
class before the class can change}

\item{runBack}{if TRUE, the dynamic prior is calculated both forward and
backward and averaged (only has an effect f \code{prior = 'dynamic'})}

\item{plot}{if TRUE, produces diagnostic plots}
}
\value{
Returns the \code{test} dataframe with new columns: "pr" = the
  predicted class membership, "[class]" = posterior probabilities per class,
  "like_[class]" = log-likelihoods, "prior_[class]" = log-priors,
  "priorF_[class]" / "priorB_[class]" = forward / backward log-priors per
  class.
}
\description{
An implementation of a Naive Bayes classifier adapted to autocorrelated time
series such as the type of nonlinear vocal phenomena in consecutive audio
frames. All predictors must be continuous, and the outcome must be
categorical. Cases with missing values are not deleted because the posterior
probabilities of each outcome class can be calculated from different
combinations of predictors on a case-by-case basis. Two optional
modifications of a standard Naive Bayes algorithm can be made: (1)
classifications can be "clumped" at the final stage, ensuring that every run
or "epoch" of a particular predicted class is at least \code{minLength} steps
long, and (2) priors can be continuously adapted based on the likelihood
function of the preceding \code{wlPrior} observations if \code{prior =
'dynamic'}.
}
\examples{
set.seed(151)
## create some fake data
df = data.frame(group = rep(c(
    rep('A', 150), rep('B', 50), rep('A', 120),
    rep('A', 100), rep('B', 30), rep('A', 90)
  ), 3))
df$group = as.factor(df$group)
df$x1 = rnorm(nrow(df), mean = ifelse(df$group == 'A', 3, 6), sd = 2)
df$x2 = rnorm(nrow(df), mean = ifelse(df$group == 'A', 2, -1), sd = 2)
boxplot(x1 ~ group, df)
boxplot(x2 ~ group, df)

## train the classifier
mod_train = naiveBayes_train(group ~ x1 + x2, data = df)
mod_train

## test on new data generated by the same process
test = data.frame(group = rep(c(
  rep('A', 90), rep('B', 40), rep('A', 150),
  rep('B', 40), rep('A', 130), rep('B', 30)
), 2))
test$group = as.factor(test$group)
test$x1 = rnorm(nrow(test), mean = ifelse(test$group == 'A', 3, 6), sd = 2)
test$x2 = rnorm(nrow(test), mean = ifelse(test$group == 'A', 2, -1), sd = 2)

# flat priors (same prior probability for each class)
nb_flat = naiveBayes(group ~ x1 + x2, train = mod_train, test = test,
  prior = 'flat', plot = TRUE)
# same as passing 'train' directly to the model, w/o calling naiveBayes_train():
nb_flat = naiveBayes(group ~ x1 + x2, train = df, test = test, prior = 'flat')
table(nb_flat$group, nb_flat$pr)
mean(nb_flat$group == nb_flat$pr) # 84\% correct

# static priors (use original class proportions as prior class probabilities)
nb_static = naiveBayes(group ~ x1 + x2, train = mod_train, test = test,
  prior = 'static', wlClumper = NULL, plot = TRUE)
table(nb_static$group, nb_static$pr)
mean(nb_static$group == nb_static$pr) # 87\% correct

# specify custom static priors
mod_train2 = mod_train
mod_train$table
mod_train2$table = list(A = .1, B = .9)  # sum to 1
nb_static2 = naiveBayes(group ~ x1 + x2, train = mod_train2, test = test,
  prior = 'static', wlClumper = NULL, plot = TRUE)
mean(nb_static2$group == nb_static2$pr)  # 61\% correct

# if we expect autocorrelation, ie class X is more likely a priori if the
# last few observations were also likely to be class X, we can use dynamic
# priors and/or clumper the predicted classes (the latter imposes strong
# constraints on the predictions, but may be worth it if the data is known to
# be strongly "clumpered", ie if we know classes occur in long'ish runs)
nb1 = naiveBayes(group ~ x1 + x2, train = mod_train, test = test,
  prior = 'dynamic', wlPrior = 10, plot = TRUE)
table(nb1$group, nb1$pr)
mean(nb1$group == nb1$pr) # 94\% correct

nb2 = naiveBayes(group ~ x1 + x2, train = mod_train, test = test,
  prior = 'static', wlClumper = 10, plot = TRUE)
table(nb2$group, nb2$pr)
mean(nb2$group == nb2$pr) # 89\% correct

nb3 = naiveBayes(group ~ x1 + x2, train = mod_train, test = test,
  prior = 'dynamic', wlPrior = 10, wlClumper = 10, plot = TRUE)
table(nb3$group, nb3$pr)
mean(nb3$group == nb3$pr) # 98\% correct

# NAs in the data are not a problem
test1 = test
test1$x1[sample(1:nrow(test1), 100)] = NA
test1$x2[sample(1:nrow(test1), 10)] = NA
summary(test1)

nb4 = naiveBayes(group ~ x1 + x2, train = mod_train, test = test,
  prior = 'dynamic', wlPrior = 10, plot = TRUE)
table(nb4$group, nb4$pr)
mean(nb4$group == nb4$pr)  # still 94\% correct
}
