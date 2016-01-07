#' Creates all combinations of 2 factor variables within a given scope and measures their relative information
#'
#' @param data A data frame
#' @param list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param var.output The name of the variable output
#' @param parallel (\emph{defaults to FALSE}) If TRUE, dispatches factor pairs in parallel
#'
#' @return A list
#' @export
#'
#' @examples
#' pairs <- list(c("cyl","factor"),c("vs","factor"),c("am","factor"),
#'               c("gear","factor"),c("carb","factor"))
#' list  <- factor.allpairs.test(mtcars,pairs,"mpg")
#' view.allpairs.test.result(list)
factor.allpairs.test <- function(data,          # a data frame
                                 list,          # a list of (varname,vartype) pairs
                                 var.output,    # a variable name
                                 parallel = FALSE) {
    apply_      <- if (parallel) { require(parallel); mclapply } else { lapply }
    all.factors <- intersect(allvariables.of.type(list,"factor"),names(data))
    all.pairs_l <- combn(all.factors, m = 2, simplify = FALSE)
    apply_(all.pairs_l,
           function(p) factor.pair.test(data = data,
                                        pair = p,
                                        var.output = var.output))
}

##########################################################################

#' A helper function to view the output of factor.allpairs.test in a table format
#'
#' @param list A list such as those created by \code{factor.allpairs.test}
#'
#' @return A data frame sorted by decreasing values of \code{RI.combined}
#' @export
#'
#' @examples
#' pairs <- list(c("cyl","factor"),c("vs","factor"),c("am","factor"),
#'               c("gear","factor"),c("carb","factor"))
#' list  <- factor.allpairs.test(mtcars,pairs,"mpg")
#' view.allpairs.test.result(list)
view.allpairs.test.result <- function(list) {
    table_ <- do.call("rbind",list)
    table_ <- table_[order(table_[["RI.combined"]],decreasing = TRUE),]
}

##########################################################################

factor.pair.test <- function(data,          # a data frame
                             pair,          # a pair of variable names
                             var.output) {  # a variable name
    var_1  <- pair[1]
    var_2  <- pair[2]
    vals_1 <- data[[var_1]]
    vals_2 <- data[[var_2]]
    newvar <- interaction(vals_1, vals_2, sep = ":", drop = TRUE)
    data2  <- data.frame(Y  = data[[var.output]],
                         X1 = vals_1,
                         X2 = vals_2,
                         XC = newvar) # probably not terribly efficient
    RI_1   <- relative.info(data = data2, var.x = "X1", var.y = "Y")
    RI_2   <- relative.info(data = data2, var.x = "X2", var.y = "Y")
    RI_C   <- relative.info(data = data2, var.x = "XC", var.y = "Y")
    data.frame(RI.combined  = RI_C,
               levels.C     = length(unique(newvar)),
               name.1       = var_1,
               levels.1     = length(unique(vals_1)),
               RI.1         = RI_1,
               name.2       = var_2,
               levels.2     = length(unique(vals_2)),
               RI.2         = RI_2
    )
}

##########################################################################

#' Adds combinations of factors to a data frame
#'
#' @param data A data frame
#' @param list.pairs A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param list.combs A list such as those created by \code{factor.allpairs.test}
#' @param thresh (\emph{defaults to 50}) The maximum number of levels that is accepted for a combination of factors to be added
#'
#' @return If \code{list.pairs} is supplied, a list whose first element is an expanded version of \code{data} and whose second element is the corresponding expanded version of \code{list.pairs}; if \code{list.pairs} is missing, returns only the modified version of \code{data}
#' @export
#'
#' @examples
#' pairs <- list(c("cyl","factor"),c("vs","factor"),c("am","factor"),
#'               c("gear","factor"),c("carb","factor"))
#' list  <- factor.allpairs.test(mtcars,pairs,"mpg")
#' newdata <- factor.add.combinations(data = mtcars, list.combs = list, thresh = 6)
#' head(newdata)
#' newdata2 <- factor.add.combinations(data = mtcars, list.pairs = pairs, list.combs = list, thresh = 6)
#' head(newdata2[[1]])
#' tail(newdata2[[2]])

factor.add.combinations <- function(data,           # a data frame
                                    list.pairs,     # a list of (varname,vartype) pairs
                                    list.combs,     # the output of factor.allpairs.test
                                    thresh = 50) {  # the maximum number of levels for a combination
    sep <- "__"
    vars_l <- lapply(list.combs, function(comb) {
        var_1  <- as.character(comb[["name.1"]])
        var_2  <- as.character(comb[["name.2"]])
        newvar <- interaction(data[[var_1]], data[[var_2]], sep = sep, drop = TRUE)
        if (length(unique(newvar)) <= thresh) {
            result <- data.frame(newvar)
            colnames(result) <- paste0(var_1,sep,var_2)
            return(result)
        } else { return() }
    })
    vars_l <- vars_l[!sapply(vars_l,is.null)]
    new.vars_ <- do.call("cbind", vars_l)
    result_1  <- data.frame(data,new.vars_)
    if (missing(list.pairs)) { return(result_1) } else {
        new.var.pairs_ <- lapply(vars_l, function(v) c(names(v),"factor"))
        result_2 <- append(list.pairs,new.var.pairs_)
        return(list(result_1,result_2))
    }
}
