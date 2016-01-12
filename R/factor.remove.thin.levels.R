# **export**
# factor.remove.thin.levels  -> a (modified) data frame
#
# **private**

#################################################################################

#' Removes thin levels from factor variables in a data frame
#'
#' @param data A data frame
#' @param vars.list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param thresh Defines the minimum number (or, if <1, the minimum proportion) of occurences of a level to be kept
#' @param tag (\emph{defaults to NA}) A value that will replace levels that do not appear in both \code{data1[[variable]]} and \code{data2[[variable]]}
#'
#' @return A modified version of data
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- data.frame(a = factor(sample(1:5,100,TRUE)),
#'                 b = factor(sample(letters[1:5],100,TRUE)))
#' table(X$a)
#' table(X$b)
#' Y <- factor.remove.thin.levels(X, thresh = 0.19, tag = "unk")
#' table(Y$a)
#' table(Y$b)
factor.remove.thin.levels <- function(data,        # a data frame
                                      vars.list,   # a list of (varname,vartype) pairs
                                      thresh = 0.001,
                                      tag = NA) {
    if (missing(vars.list)) {
        vars.list <- lapply(names(data), function(var) c(var, class(data[[var]])))
    }
    data.variables <- names(data)
    limit <- if (thresh < 1) { nrow(data) * thresh } else { thresh }
    for (var.pair in vars.list) {
        varname <- var.pair[1]
        vartype <- var.pair[2]
        if (!varname %in% data.variables || vartype != "factor") { next }
        values <- data[[varname]]
        levels <- levels(values)
        table  <- table(values)
        skinny.levels <- (table < limit)
        if (!is.na(tag) && any(skinny.levels)) {
            levels(data[[varname]]) <- c(levels,tag)
        }
        data[values %in% levels[skinny.levels],varname] <- tag
        data[[varname]] <- droplevels(data[[varname]])
    }
    return(data)
}
