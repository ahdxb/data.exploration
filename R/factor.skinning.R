#' Removes thin levels from factor variables in a data frame
#'
#' @param data A data frame
#' @param list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param thresh Defines the minimum number (or, if <1, the minimum proportion) of occurences of a level to be kept
#' @param tag A value that will replace levels that do not appear in both \code{data1[[variable]]} and \code{data2[[variable]]}
#'
#' @return A modified version of data
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- data.frame(a = factor(sample(1:5,100,TRUE)),
#'                 b = factor(sample(letters[1:5],100,TRUE)))
#' pairslist <- list(c("a","factor"), c("b","factor"))
#' Y <- factor.skinning(X, pairslist, thresh = 0.19)
#' table(Y$a)
#' table(Y$b)
factor.skinning <- function(data,        # a data frame
                            list,        # a list of (varname,vartype) pairs
                            thresh = 0.001,
                            tag = NA) {
    data.variables <- names(data)
    limit <- if (thresh < 1) { nrow(data) * thresh } else { thresh }
    for (var.pair in list) {
        varname <- var.pair[1]
        vartype <- var.pair[2]
        if (!varname %in% data.variables || vartype != "factor") { next }
        levels <- levels(data[[varname]])
        table  <- table(data[[varname]])
        skinny.levels <- (table < limit)
        if (!is.na(tag) && any(skinny.levels)) {
            levels(data[[varname]]) <- c(levels(data[[varname]]),tag)
        }
        data[data[[varname]] %in% levels[skinny.levels],varname] <- tag
    }
    return(data)
}
