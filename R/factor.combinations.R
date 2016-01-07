#' Combines two factor variables and measures the new variable's relative info
#'
#' @param data A data frame
#' @param pair A pair of variable names
#' @param var.output A variable name
#'
#' @return A data frame with each input variable and the combined variable's number of unique values and relative information
#' @export
#'
#' @examples
factor.combinations.test <- function(data,          # a data frame
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
    RI_1     <- relative.info(data = data2, var.x = "X1", var.y = "Y")
    RI_2     <- relative.info(data = data2, var.x = "X2", var.y = "Y")
    RI_C     <- relative.info(data = data2, var.x = "XC", var.y = "Y")
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
