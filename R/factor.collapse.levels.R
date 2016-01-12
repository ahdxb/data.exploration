# **export**
# factor.collapse.levels  ->  a list of two (modified) data frames
#
# **private**

#################################################################################

#' Reduces common factor variables of two datasets to common levels
#'
#' @param data1 A data frame
#' @param data2 Another data frame
#' @param vars.list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param tag (\emph{defaults to NA}) A value that will replace levels that do not appear in both \code{data1[[variable]]} and \code{data2[[variable]]}
#'
#' @return A list of two modified data frames (\code{list(newdata1,newdata2)})
#' @export
#'
#' @examples
#' X <- data.frame(a = factor(sample(1:3,100,TRUE)),
#'                 b = factor(sample(letters[1:5],100,TRUE)))
#' Y <- data.frame(a = factor(sample(2:4,50,TRUE)),
#'                 b = factor(sample(letters[3:7],50,TRUE)))
#' Z <- factor.collapse.levels(X,Y)
#' head(cbind(X,Y,Z[[1]],Z[[2]]), 20)
factor.collapse.levels <- function(data1,      # a data frame
                                   data2,      # a second data frame
                                   vars.list,  # a list of (varname,vartype) pairs
                                   tag = NA) {
    data.variables <- intersect(names(data1),names(data2))
    if (missing(vars.list)) {
        vars.list <- lapply(data.variables, function(var) c(var, class(data1[[var]])))
    }
    for (var.pair in vars.list) {
        varname <- var.pair[1]
        vartype <- var.pair[2]
        if (!varname %in% data.variables || vartype != "factor") { next }
        data1.levels  <- droplevels(data1[[varname]])
        data2.levels  <- droplevels(data2[[varname]])
        if (setequal(data1.levels,data2.levels)) { next }
        common.levels <- intersect(data1.levels, data2.levels)
        if (!is.na(tag)) {
            levels(data1[[varname]]) <- c(levels(data1[[varname]]),tag)
            levels(data2[[varname]]) <- c(levels(data2[[varname]]),tag)
        }
        data1[!data1[[varname]] %in% common.levels,varname] <- tag
        data2[!data2[[varname]] %in% common.levels,varname] <- tag
        data1[[varname]] <- droplevels(data1[[varname]])
        data2[[varname]] <- droplevels(data2[[varname]])
    }
    return(list(data1,data2))
}
