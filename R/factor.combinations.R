# **export**
# factor.allpairs.test          -> a data frame or a list of 1-line data frames
# factor.select.combinations    -> a (modified) data frame or list
# factor.add.combinations       -> a (modified) data frame or a list with a df and a list
#
# **private**
# factor.pair.test
# sep

#################################################################################

#' Creates all 2-combinations of factor variables within a given scope and measures their relative information to an output variable
#'
#' @param data A data frame
#' @param vars.list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param var.output The name of the variable output
#' @param parallel (\emph{defaults to FALSE}) If TRUE, dispatches factor pairs in parallel
#' @param return.list (\emph{defaults to TRUE}) If \code{TRUE}, the function returns a list of one-line dataframes; if \code{FALSE}, it returns a concatenated dataframe sorted by decreasing value of \code{RI.combined}
#'
#' @return A list
#' @export
#'
#' @examples
#' list.pairs <- list(c("cyl","factor"),c("vs","factor"),c("am","factor"),
#'                    c("gear","factor"),c("carb","factor"))
#' factor.allpairs.test(mtcars,list.pairs,"mpg")
factor.allpairs.test <- function(data,          # a data frame
                                 vars.list,     # a list of (varname,vartype) pairs
                                 var.output,    # a variable name
                                 return.list = FALSE,
                                 parallel = FALSE) {
    if (missing(vars.list)) {
        vars.list <- lapply(names(data), function(var) c(var, class(data[[var]])))
    }
    apply_      <- if (parallel) { require(parallel); mclapply } else { lapply }
    all.factors <- setdiff(intersect(allvariables.of.type(vars.list,"factor"),
                                     names(data)),
                           var.output)
    all.pairs_l <- combn(all.factors, m = 2, simplify = FALSE)
    result <- apply_(all.pairs_l,
                     function(p) factor.pair.test(data = data,
                                                  pair = p,
                                                  var.output = var.output))
    if(!return.list) {
        result <- do.call(rbind,result)
        result <- result[order(result[["RI.combined"]],decreasing = TRUE),]
    }
    return(result)
    }

##########################################################################

factor.pair.test <- function(data,          # a data frame
                             pair,          # a pair of variable names
                             var.output) {  # a variable name
    sep    <- sep()
    var_1  <- pair[1]
    var_2  <- pair[2]
    vals_1 <- data[[var_1]]
    vals_2 <- data[[var_2]]
    newvar <- interaction(vals_1, vals_2, sep = ":", drop = TRUE)
    data2  <- data.frame(Y  = data[[var.output]],
                         X1 = vals_1,
                         X2 = vals_2,
                         XC = newvar) # probably not a terribly efficient way to do it ...
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

#' A helper function to filter a list of combinations
#'
#' @param combinations A list or a data frame such as those created by \code{factor.allpairs.test}
#' @param max.factors An integer
#' @param min.RI A numeric
#'
#' @return A modified version of \code{combinations}
#' @export
#'
#' @examples
#' list.pairs <- list(c("cyl","factor"),c("vs","factor"),c("am","factor"),
#'                    c("gear","factor"),c("carb","factor"))
#' list.combs <- factor.allpairs.test(mtcars,list.pairs,"mpg",TRUE)
#' factor.select.combinations(list.combs, max.factors = 6, min.RI = 0.4)
#' df.combs   <- factor.allpairs.test(mtcars,list.pairs,"mpg")
#' factor.select.combinations(df.combs, max.factors = 6, min.RI = 0.4)
factor.select.combinations <- function(combinations,  # an output of factor.allpairs.test
                                       max.factors,   # an integer
                                       min.RI) {      # a numeric in (0,1)
    if (missing(max.factors)) { max.factors <- Inf }
    if (missing(min.RI)) { min.RI <- 0 }
    result <- if (class(combinations) == "list") {
        Filter(Negate(is.null),
               lapply(combinations, function(comb) {
                   if ((comb[["levels.C"]] <= max.factors) &&
                       (comb[["RI.combined"]] >= min.RI)) { comb }
               }))
    } else if (class(combinations) == "data.frame") {
        combinations[combinations[["levels.C"]] <= max.factors &
                         combinations[["RI.combined"]] >= min.RI,]
    } else { stop("Input 'combinations' has the wrong type") }
    return(result)
}

##########################################################################

#' Adds combinations of factors to a data frame
#'
#' @param data A data frame
#' @param vars.list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param list.combs A list such as those created by \code{factor.allpairs.test}
#'
#' @return If \code{vars.list} is supplied, a list whose first element is an expanded version of \code{data} and whose second element is the corresponding expanded version of \code{vars.list}; if \code{vars.list} is missing, returns only the modified version of \code{data}
#' @export
#'
#' @examples
#' list.pairs <- list(c("cyl","factor"),c("vs","factor"),c("am","factor"),
#'                    c("gear","factor"),c("carb","factor"))
#' list.combs  <- factor.allpairs.test(mtcars,pairs,"mpg")
#' view.allpairs.test.result(list.combs)
#' newdata <- factor.add.combinations(data = mtcars, list.combs = list.combs)
#' head(newdata)
#' list.combs2 <- select.combinations(list.combs, max.factors = 6, min.RI = 0.4)
#' newdata2 <- factor.add.combinations(data = mtcars, vars.list = list.pairs, list.combs = list.combs2)
#' head(newdata2[[1]])
#' tail(newdata2[[2]])
factor.add.combinations <- function(data,            # a data frame
                                    vars.list,       # a list of (varname,vartype) pairs
                                    combinations) {  # an output of factor.allpairs.test
    sep <- sep()
    # we force combinations into a list to make flow simpler
    comb_list <- if(class(combinations) == "list") { combinations } else {
        lapply(1:nrow(combinations), function(i) combinations[i,])
    }
    newvars_list <- lapply(comb_list, function(comb) {
        var_1  <- as.character(comb[["name.1"]])
        var_2  <- as.character(comb[["name.2"]])
        newvar <- interaction(data[[var_1]], data[[var_2]], sep = sep, drop = TRUE)
        result <- data.frame(newvar)
        colnames(result) <- paste0(var_1,sep,var_2)
        return(result)
    })
    newvars_df <- do.call(cbind, newvars_list)
    result_df  <- cbind(data,newvars_df)
    if (missing(vars.list)) { return(result_df) } else {
        newvar_pairs <- lapply(newvars_list, function(v) c(names(v),"factor"))
        result_varlist <- append(vars.list,newvar_pairs)
        return(list(result_df,result_varlist))
    }
}

##########################################################################

sep <- function() "__"
