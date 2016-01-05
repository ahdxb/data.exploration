#' Measuring relative information between factors and output variable
#'
#' @param data A data frame
#' @param var.output The name of the variable output
#' @param list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#'
#' @return A data frame with two columns: variable names, and their relative information to the output variable
#' @export
#'
#' @examples
#' X <- data.frame(a = factor(sample(1:3,100,TRUE)),
#'                 b = factor(sample(1:5,100,TRUE)),
#'                 c = factor(sample(letters[1:5],100,TRUE)))
#' pairslist <- list(c("a","factor"), c("b","integer"), c("c","factor"))
#' factor.factor.info(X, "a", pairslist)
factor.factor.info <- function(data,         # a data frame
                               var.output,   # the name of the output variable
                               list) {       # a list of (varname,vartype) pairs
    data.variables <- setdiff(names(data),var.output)
    result <- lapply(list,
                     function(var.pair) {
                         varname <- var.pair[1]
                         if (!varname %in% data.variables) { return() }
                         vartype <- var.pair[2]
                         if (vartype == "factor") {
                             c(varname,
                               relative.info(data = data,
                                             var.x = varname,
                                             var.y = var.output))
                         }
                     })
    result <- data.frame(do.call("rbind",result))
    colnames(result) <- c("variable", "rel.info.to.output")
    result[order(result[["rel.info.to.output"]], decreasing = TRUE),]
}
