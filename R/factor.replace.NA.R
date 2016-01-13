#' Replace NA in factor variables
#'
#' @param data A data frame
#' @param tag A string
#'
#' @return A modified version of \code{data}
#' @export
#'
#' @examples
#' X <- data.frame(a = c(1,2,NA), b = c("a",NA,"c"))
#' factor.replace.NA(X,"unk")
factor.replace.NA <- function(data,    # a data set
                              tag) {   # a string
    for (var in names(data)) {
        values <- data[[var]]
        if (class(values) != "factor") { next }
        NAs <- is.na(values)
        if (!any(NAs)) { next }
        levels(data[[var]]) <- c(levels(values),tag)
        data[NAs,var] <- tag
    }
    return(data)
}
