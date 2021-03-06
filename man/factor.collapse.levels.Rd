% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor.collapse.levels.R
\name{factor.collapse.levels}
\alias{factor.collapse.levels}
\title{Reduces common factor variables of two datasets to common levels}
\usage{
factor.collapse.levels(data1, data2, vars.list, tag = NA)
}
\arguments{
\item{data1}{A data frame}

\item{data2}{Another data frame}

\item{vars.list}{A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}}

\item{tag}{(\emph{defaults to NA}) A value that will replace levels that do not appear in both \code{data1[[variable]]} and \code{data2[[variable]]}}
}
\value{
A list of two modified data frames (\code{list(newdata1,newdata2)})
}
\description{
Reduces common factor variables of two datasets to common levels
}
\examples{
X <- data.frame(a = factor(sample(1:3,100,TRUE)),
                b = factor(sample(letters[1:5],100,TRUE)))
Y <- data.frame(a = factor(sample(2:4,50,TRUE)),
                b = factor(sample(letters[3:7],50,TRUE)))
Z <- factor.collapse.levels(X,Y)
head(cbind(X,Y,Z[[1]],Z[[2]]), 20)
}

