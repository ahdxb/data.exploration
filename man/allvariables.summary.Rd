% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/____allvariables.summary____old____.R
\name{allvariables.summary}
\alias{allvariables.summary}
\title{Summary of all variables within train and test data}
\usage{
allvariables.summary(train.set, test.set, var.output, digits = 3,
  parallel = FALSE)
}
\arguments{
\item{train.set}{The \emph{train} data (a data frame)}

\item{test.set}{The \emph{test} data (a data frame)}

\item{var.output}{(\strong{optional}) The \emph{output} variable (has to be a variable in \code{train.set})}

\item{digits}{(defaults to \code{3}) Used for rounding of relative info}

\item{parallel}{(defaults to \code{FALSE}) If \code{TRUE}, uses \code{mclapply} to list over variables}
}
\value{
A data frame which each row a variable in \code{train.set}
}
\description{
Summary of all variables within train and test data
}
\examples{
allvariables.summary(iris)
allvariables.summary(iris, var.output = "Sepal.Length")
set.seed(123)
iris_train <- iris[sample(150,50),-1]  # remove Sepal.Length
iris_test <- iris[sample(150,50),-2]   # remove Sepal.Width
allvariables.summary(iris_train,iris_test)
allvariables.summary(iris_train,iris_test,"Species")
}

