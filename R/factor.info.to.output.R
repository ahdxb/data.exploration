# **export**
# factor.info.to.output    -> a data frame or a list of 1-line data frames
#
# **private**

#################################################################################

#' Measuring relative information between factors and output variable
#'
#' @param data A data frame
#' @param var.output The name of the variable output
#' @param vars.list (\emph{optional}) A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param return.list (\emph{defaults to TRUE}) If \code{TRUE}, the function returns a list of one-line dataframes; if \code{FALSE}, it returns a concatenated dataframe sorted by decreasing value of \code{RI.to.output}
#'
#' @return A data frame with three columns: variable names, the number of active levels, and their relative information to the output variable
#' @export
#'
#' @examples
#' X <- data.frame(a = factor(sample(1:3,100,TRUE)),
#'                 b = factor(sample(1:5,100,TRUE)),
#'                 c = factor(sample(letters[1:5],100,TRUE)))
#' factor.info.to.output(X, "a")
#' list_ <- list(c("vs","factor"),c("am","factor"),c("gear","factor"))
#' factor.info.to.output(mtcars, "cyl", list_, return.list = TRUE)
factor.info.to.output <- function(data,         # a data frame
                                  var.output,   # the name of the output variable
                                  vars.list,    # a list of (varname,vartype) pairs
                                  return.list = FALSE) {
    if (missing(vars.list)) {
        vars.list <- lapply(names(data), function(var) c(var, class(data[[var]])))
    }
    data.variables <- setdiff(names(data),var.output)
    describe.func_ <- function(var.pair) {
        varname <- var.pair[1]
        vartype <- var.pair[2]
        if (!varname %in% data.variables || vartype != "factor") { return() }
        data.frame(name         = varname,
                   levels       = length(unique(data[[varname]])),
                   RI.to.output = relative.info(data = data,
                                                var.x = varname,
                                                var.y = var.output),
                   stringsAsFactors = FALSE)
    }
    result <- lapply(vars.list, describe.func_)
    if (!return.list) {
        result <- data.frame(do.call(rbind,result))
        result <- result[order(result[["RI.to.output"]], decreasing = TRUE),]
    }
    return(result)
}
