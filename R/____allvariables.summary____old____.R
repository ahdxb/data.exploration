#' Summary of all variables within train and test data
#'
#' @param train.set The \emph{train} data (a data frame)
#' @param test.set The \emph{test} data (a data frame)
#' @param var.output (\strong{optional}) The \emph{output} variable (has to be a variable in \code{train.set})
#' @param digits (defaults to \code{3}) Used for rounding of relative info
#' @param parallel (defaults to \code{FALSE}) If \code{TRUE}, uses \code{mclapply} to list over variables
#'
#' @return A data frame which each row a variable in \code{train.set}
#'
#' @examples
#' allvariables.summary(iris)
#' allvariables.summary(iris, var.output = "Sepal.Length")
#' set.seed(123)
#' iris_train <- iris[sample(150,50),-1]  # remove Sepal.Length
#' iris_test <- iris[sample(150,50),-2]   # remove Sepal.Width
#' allvariables.summary(iris_train,iris_test)
#' allvariables.summary(iris_train,iris_test,"Species")
allvariables.summary <- function(train.set, test.set, var.output,
                                 digits = 3, parallel = FALSE) {
    if (missing(test.set)) {test.set <- train.set}
    if (missing(var.output)) {
        allvariables.summary.without.output(train.set = train.set, test.set = test.set,
                                            parallel = parallel)
    } else {
        allvariables.summary.with.output(train.set = train.set, test.set = test.set,
                                         var.output = var.output,
                                         digits = digits, parallel = parallel)
    }
}

allvariables.summary.with.output <- function(train.set, test.set, var.output,
                                             digits = 3, parallel = FALSE) {
    temp_ <- allvariables.summary.without.output(train.set = train.set,
                                                 test.set = test.set,
                                                 parallel = parallel)
    format_ <- function(...) format(..., digits = digits, nsmall = digits)
    apply_  <- if (parallel) mclapply else lapply
    ri_ <- sapply(1:nrow(temp_), function(i) {
        if (temp_[i,"train.values"] == 0) {NA}
        else {format_(relative.info(data  = train.set,
                                    var.x = as.character(temp_[i,"name"]),
                                    var.y = var.output))}
    })
    temp_ <- data.frame(temp_, train.relinf = ri_)
    temp_[ order(temp_[["train.relinf"]], decreasing = TRUE), ]
}

allvariables.summary.without.output <- function(train.set, test.set, parallel = FALSE) {
    #### required libs ####
    if (parallel) { require(parallel) }
    #### error checking ####
    # none?
    #### function body ####
    apply_   <- if (parallel) mclapply else lapply
    all.vars <- union(names(train.set),names(test.set))
    list     <- apply_(all.vars,
                       function(v) describe.class(train.set,test.set,v))
    data.frame( do.call("rbind",list) )
}

describe.var.factor  <- function(train.set, test.set, var) {
    train.values <- train.set[[var]]
    train.class  <- class(train.values)
    test.values  <- test.set[[var]]
    test.class   <- class(test.values)
    class_ <- if (is.null(train.values)) { c(test.class,FALSE) }
              else {
                  if (is.null(test.values)) { c(train.class,FALSE) }
                  else {
                      if (train.class == test.class) { c(train.class,FALSE) }
                      else { c(paste0(train.class, "|", test.class),TRUE) }
                  }
              }
    data.frame(name  = var,
               train = length(unique(train.values)),
               test  = length(unique(test.values)),
               class = class_[1],
               class.diff = class_[2]
    )
}
