#' Changing a data frame's variable types in bulk
#'
#' @param data A data frame
#' @param list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param data2 A second data frame (optional) from which values will be pulled when coercing a variable into a factor
#'
#' @return A modified version of \code{data}
#' @export
#'
#' @examples
allvariables.type.change <- function(data,   # a data frame
                                 list,       # a list of (varname,vartype) pairs
                                 data2) {    # another data frame (optional)
    data.variables <- names(data)
    for (var.pair in list) {
        varname <- var.pair[1]
        if (!varname %in% data.variables) { next }
        vartype <- var.pair[2]
        if (class(data[[varname]]) != vartype) {
            data[[varname]] <- variable.type.change(data,varname,vartype,data2)
        }
    }
    return(data)
}

variable.type.change <- function(data, varname, vartype, data2) {
    if (vartype == "factor") {
        if (missing(data2)) {
            return(as.factor(data[[varname]]))
        } else {
            return(factor(data[[varname]],
                          levels = union(unique(data[[varname]]),
                                         unique(data2[[varname]]))
                              ))
        }
    } else if (vartype == "numeric") {
        return(as.numeric(data[[varname]]))
    } else if (vartype == "integer") {
        return(as.integer(data[[varname]]))
    } else if (vartype == "character") {
        return(as.character(data[[varname]]))
    } else if (vartype == "Date") {
        return(as.Date(data[[varname]]))
    } else if (vartype == "unclear") {
        warning(paste0("Variable ",varname," has commanded type 'unclear'"))
        return(data[[varname]])
    } else {
        stop(paste0("Bug: ",vartype," is not a recognized type."))
    }
}


