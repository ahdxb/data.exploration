#' Summary of all variables within train and test data
#'
#' @param train.set The \emph{train} data (a data frame)
#' @param test.set The \emph{test} data (a data frame)
#' @param var.output (\strong{optional}) The \emph{output} variable (has to be a variable in \code{train.set})
#' @param digits (defaults to \code{3}) Used for rounding of relative info
#' @param parallel (defaults to \code{FALSE}) If \code{TRUE}, uses \code{mclapply} to list over variables
#'
#' @return A data frame which each row a variable in \code{train.set}
#' @export
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
    apply_     <- if (parallel) mclapply else lapply
    describe_  <- function(var) {
        train.values <- train.set[[var]]
        train.class  <- class(train.values)
        test.values  <- test.set[[var]]
        test.class   <- class(test.values)
        class_ <- if (is.null(train.values)) { paste0("0|", test.class) }
                  else {
                      if (is.null(test.values)) { paste0(train.class, "|0") }
                      else {
                          if (train.class == test.class) { train.class }
                          else { paste0(train.class, "|", test.class, " !!") }
                      }
                  }
        data.frame(name = var,
                   train.values = length(unique(train.values)),
                   test.values  = length(unique(test.values)),
                   class = class_
        )
    }
    train.vars <- names(train.set)
    test.vars  <- names(test.set)
    list <- apply_(X = union(train.vars,test.vars), FUN = describe_)
    concat <- data.frame( do.call("rbind",list) )
    concat[ order(concat[[2]], decreasing = TRUE), ]
}
