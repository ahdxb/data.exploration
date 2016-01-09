# Functions:
#
# **export**
# allvariables.manual.review    -> a list of pairs (varname, vartype)
# allvariables.of.type          -> a vector of variable names
# allvariables.type.change      -> a modified data frame
# change.variable.type.in.list  -> a modified list of pairs (varname, vartype)
#
# **private**
# all.types               -> a vector of variable type names
# variable.manual.review  -> a pair (varname, vartype)
# variable.type.change    -> a vector (column) of data

#################################################################################

#' Manual review of a data frame's variables
#'
#' @param data A data frame
#' @param clear.console (\emph{defaults to TRUE}) If TRUE, the console is cleared before each new variable's summary is printed
#' @param max.unique.factor (\emph{defaults to 50}) When a variable's number of unique values is lower than this, the guessed type is \code{factor}
#'
#' @return A list of pairs (variable,type)
#' @export
#'
#' @examples
#' allvariables.manual.review(iris)   # all default guesses are good
#' allvariables.manual.review(mtcars) # some variables should be re-classified as integer or factor
allvariables.manual.review <- function(data,  # a data frame
                                       clear.console = TRUE,
                                       max.unique.factor = min(50,nrow(data)/10)) {
    lapply(names(data),
           function(v) variable.manual.review(data,v,clear.console,max.unique.factor))
}

#################################################################################

all.types <- function() c("factor",
                          "numeric",
                          "integer",
                          "character",
                          "Date",
                          "unclear")

#################################################################################

variable.manual.review <- function(data,  # a data frame
                                   var,   # a variable name
                                   clear.console = TRUE,
                                   max.unique.factor) {
    TYPES  <- all.types()
    NTYPES <- length(TYPES)
    values <- data[[var]]
    guess  <- if (length(unique(values)) <= max.unique.factor) {
                  "factor"
              } else {
                  class(data[[var]])
              }
    guess.index <- if (guess %in% TYPES) { which(guess == TYPES) }
                   else { NTYPES }
    table  <- data.frame(type  = TYPES,
                         guess = factor("", levels = c("","*")))
    table[guess.index, "guess"] <- "*"

    prompt_ <- "Enter type (type nothing for default guess, X for unclear and D, S or T for more detail): "
    read <- "-1"

    while (!(read %in% c("","X") || as.integer(read) %in% 1:NTYPES)) {
        if (clear.console) { cat("\014") }
        print(paste0("Variable: ",var))
        if (read == "D") {
            str(sample(data[[var]]), vec.len = 20)
        } else if (read == "S") {
            print(summary(data[[var]]))
        } else if (read == "T") {
            table_ <- table(data[[var]])
            table_ <- table_[order(table_, decreasing = TRUE)]
            lengt_ <- length(table_)
            if (lengt_ <= max.unique.factor) { print(table_) } else {
                print(table_[1:max.unique.factor])
                print(paste0("Variable ",var, " has too many unique values (",
                             lengt_,"); showing only the first ", max.unique.factor))
            }
        } else {
            str(data[[var]], vec.len = 8)
        }
        print(table, right = FALSE)
        read <- readline(prompt_)
    }

    type <- if (read == "") { TYPES[guess.index] }
            else if (read == "X") { TYPES[NTYPES] }
            else {TYPES[as.integer(read)]}
    return(c(var,type))
}

#################################################################################

#' Extracts all variables of a given from a pair list
#'
#' @param list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param type The name of a type, e.g. \code{"numeric"}
#'
#' @return A vector of variable names
#' @export
#'
#' @examples
#' list <- lapply(names(mtcars), function(v) c(v,class(mtcars[[v]])))
#' allvariables.of.type(list, "numeric")
allvariables.of.type <- function(list,    # a list of (varname,vartype) pairs
                                 type) {  # a type name
    unlist(lapply(list, function(pair) if (pair[2] == type) return(pair[1])))
}

#################################################################################

#' Manually changes an element in a (name,type) list
#'
#' @param list A list of pairs (variable.name,variable.type) such as those produced by \code{allvariables.manual.review}
#' @param varname A variable name
#' @param vartype A variable type
#'
#' @return A modified version of \code{list}
#' @export
#'
#' @examples
#' list <- lapply(names(mtcars), function(v) c(v,class(mtcars[[v]])) )
#' change.variable.type.in.list(list, "am", "factor")
change.variable.type.in.list <- function(list,       # a list of (varname,vartype) pairs
                                         varname,    # a variable name
                                         vartype) {  # a variable type
    lapply(list, function(pair) {
        if (pair[1] == varname) { c(varname,vartype) } else { pair }
    })
}

#################################################################################

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
allvariables.type.change <- function(data,    # a data frame
                                     list,    # a list of (varname,vartype) pairs
                                     data2) { # another data frame (optional)
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

#################################################################################

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

