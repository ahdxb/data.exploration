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
                                       max.unique.factor = 50) {
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
                                   max.unique.factor = 50) {
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

    prompt_ <- "Enter type (type nothing for default guess, X for unclear and 0, S or T for more detail): "
    read <- "-1"

    while (!(read %in% c("","X") || as.integer(read) %in% 1:NTYPES)) {
        if (clear.console) { cat("\014") }
        print(paste0("Variable: ",var))
        if (read == "0") {
            str(sample(data[[var]]), vec.len = 20)
        } else if (read == "S") {
            print(summary(data[[var]]))
        } else if (read == "T") {
            print(table(data[[var]]))
        } else {
            str(data[[var]])
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
allvariables.of.type <- function(list,    # a list of (varname,vartype) pairs
                                 type) {  # a type name
    unlist(lapply(list, function(pair) if (pair[2] == type) return(pair[1])))
}
