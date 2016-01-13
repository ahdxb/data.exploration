# **export**
# relative.info
# penalized.relative.info
#
# **private**
# total.relative.info
# pointwise.relative.info

#################################################################################

#' Relative information
#'
#' @param data A data frame
#' @param var.x The \emph{regressor} (or \emph{predictor}) variable (has to be the name of a variable in \code{data})
#' @param var.y The \emph{output} variable (has to be the name of a variable in \code{data})
#' @param x.value (\strong{optional}) The value of \code{var.x} to condition upon (has to be a value of \code{data[[var.x]]})
#'
#' @return \itemize{
#' \item If \code{x.value} \strong{is} specified, returns
#' PRI(var.y|var.x = x.value) = 1 - H(var.y|var.x = x.value)/H(var.y).
#' \item If \code{x.value} \strong{is not} specified, returns RI(var.y|var.x) = MI(var.x,var.y)/H(var.y)
#' }
#' @export
#'
#' @examples
#' relative.info(data = mtcars, var.x = "cyl", var.y = "mpg")
#' relative.info(data = mtcars, var.x = "cyl", var.y = "mpg", x.value = 4)
relative.info <- function(data, var.x, var.y, x.value) {
    if (missing(x.value)) {
        total.relative.info(data = data, var.x = var.x, var.y = var.y)
    } else {
        pointwise.relative.info(data = data, var.x = var.x, var.y = var.y, x.value = x.value)
    }
}

#################################################################################

total.relative.info <- function(data, var.x, var.y) {
    #### required libs ####
    require(entropy)
    #### error checking ####
    data.names <- names(data)
    if (!var.x %in% data.names) {
        stop("var.x (", substitute(var.x), ") is not a variable of data (", substitute(data), ")")
    }
    if (!var.y %in% data.names) {
        stop("var.y (", substitute(var.y), ") is not a variable of data (", substitute(data), ")")
    }
    #### function body ####
    mi_x_y <- mi.empirical( table( data[ , c(var.x,var.y)] ))
    entr_y <- entropy.empirical( table( data[[var.y]] ))
    return(mi_x_y / entr_y)
}

#################################################################################

pointwise.relative.info <- function(data, var.x, var.y, x.value) {
    #### required libs ####
    require(entropy)
    #### error checking ####
    data.names <- names(data)
    x <- data[[var.x]]
    if (!var.x %in% data.names) {
        stop("var.x (", substitute(var.x), ") is not a variable of data (", substitute(data), ")")
    }
    if (!var.y %in% data.names) {
        stop("var.y (", substitute(var.y), ") is not a variable of data (", substitute(data), ")")
    }
    if (!x.value %in% unique(x)) {
        stop("x.value (", substitute(x.value), ") is not a value of data[[var.x]] (",
             "data: ", substitute(data), "var.x: ", substitute(var.x), ")")
    }
    #### function body ####
    y_given_x <- data[x == x.value, var.y]
    y <- data[[var.y]]
    entr_y <- entropy.empirical( table( y ))
    entr_y_given_x <- entropy.empirical( table( y_given_x ))
    return(1 - entr_y_given_x / entr_y)
}

# Some examples
#
# Sum of pointwise relative info PRI(mpg|cyl=x) for all values of x (data = mtcars): 0.308163
# sum(sapply(unique(mtcars$cyl),
#            function(c) sum(mtcars$cyl == c)/nrow(mtcars) *
#                relative.info(data = mtcars, var.x = "cyl", var.y = "mpg", x.value = c)))
# Relative info RI(mpg|cyl): 0.308163
# relative.info(data = mtcars, var.x = "cyl", var.y = "mpg")
#
# Sum of pointwise relative info PRI(Sepal.Width|Species=x) for all values of x (data = iris): 0.1284165
# sum(sapply(unique(iris$Species),
#            function(s) sum(iris$Species == s)/nrow(iris) *
#                relative.info(data = iris, var.x = "Species", var.y = "Sepal.Width", x.value = s)))
# Relative info RI(Sepal.Width|Species): 0.1284165
# relative.info(data = iris, var.x = "Species", var.y = "Sepal.Width")
#
# Synthetic dataset
# D1 <- data.frame(K = 1, Y = sample(1:2, 100, T))
# D2 <- data.frame(K = 2, Y = sample(2:4, 100, T))
# D3 <- data.frame(K = 3, Y = sample(4:7, 100, T))
# D <- rbind(D1,D2,D3)
# plot(table(D$Y))
# relative.info(D, "K", "Y")  # 0.42
# relative.info(data = D, var.x = "K", var.y = "Y", x.value = 1)  # 0.62
# relative.info(data = D, var.x = "K", var.y = "Y", x.value = 2)  # 0.40
# relative.info(data = D, var.x = "K", var.y = "Y", x.value = 3)  # 0.24
#

#################################################################################

#' Penalized relative information
#'
#' @param data A data frame
#' @param var.x The \emph{regressor} (or \emph{predictor}) variable (has to be the name of a variable in \code{data})
#' @param var.y The \emph{output} variable (has to be the name of a variable in \code{data})
#' @param alpha (\emph{defaults to 2}) A numeric
#'
#' @return PenRI(var.y|var.x;alpha) = Sum_(a ~ var.x) P(a)^(alpha) * PointRI(var.y|var.x = a)
#' @export
#'
#' @examples
#' list.pairs <- list(c("cyl","factor"),c("am","factor"),c("vs","factor"),c("gear","factor"))
#' mtcars2 <- allvariables.type.change(mtcars, list.pairs)
#' penalized.relative.info(mtcars2,"cyl","gear",3/2)
penalized.relative.info <- function(data, var.x, var.y, alpha = 2) {
    require(entropy)
    data.nrow    <- nrow(data)
    values.var.x <- data[[var.x]]
    table.data.x <- table(values.var.x)
    sum(sapply(unique(values.var.x),
                   function(s) {
                       proba <- as.numeric(table.data.x[as.character(s)]/data.nrow)
                       proba ^ alpha * pointwise.relative.info(data, var.x, var.y, x.value = s)
               }))
}
