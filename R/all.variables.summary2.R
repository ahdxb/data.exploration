
all.types <- function() c("factor",
                          "numeric",
                          "integer",
                          "character",
                          "Date",
                          "unclear")

variable.manual.review <- function(data,  # a data frame
                                   var,   # a variable name
                                   clear.console = TRUE) {
    TYPES  <- all.types()
    NTYPES <- length(TYPES)
    guess  <- class(data[[var]])
    guess.index <- if (guess %in% TYPES) { which(guess == TYPES) }
                   else { NTYPES }
    table  <- data.frame(type  = TYPES,
                         guess = factor("", levels = c("","*")))
    table[guess.index, "guess"] <- "*"

    prompt_ <- "Enter type (type nothing for default guess, X for exit and 0 for more detail): "
    prompt.error_ <- "Wrong entry. Please try again: "
    read <- "-1"

    while (!(read == "X" || read == "" || as.integer(read) %in% 1:NTYPES)) {
        if (clear.console) { cat("\014") }
        print(paste0("Variable: ",var))
        if (read == "0") { str(sample(data[[var]]), vec.len = 20) }
        else { str(data[[var]]) }
        print(table, right = FALSE)
        read <- readline(prompt_)
    }
    if (read == "X") {stop("Manual exit.")}

    type <- if (read == "") {TYPES[guess.index]} else {TYPES[as.integer(read)]}
    return(c(var,type))
}

allvariables.manual.review <- function(data,  # a data frame
                                       clear.console = TRUE) {
    lapply(names(data), function(v) variable.manual.review(data,v,clear.console))
}
