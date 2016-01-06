
onevariable.summary.without.output <- function(var.x, train.set, test.set,
                                               digits = 3) {
    #### required libs ####
    # none
    #### error checking ####
    # >>> todo
    #### function body ####
    if (missing(test.set)) {test.set <- train.set}
    #
    train.values <- train.set[[var.x]]
    train.table  <- table(train.values)
    train.values.freqs <- train.table / nrow(train.set)
    train.values.names <- as.factor(names(train.table))
    #
    test.values  <- test.set[[var.x]]
    test.values.freqs  <- table(test.values) / nrow(test.set)
    test.values.names  <- as.factor(unique(test.values))
    #
    all.values.names   <- union(train.values.names,test.values.names)
    #
    result <- data.frame(value      = all.values.names,
                         freq.train = train.values.freqs[all.values.names],
                         freq.test  = test.values.freqs[all.values.names])
    result <- result[ order(result$freq.train, decreasing = TRUE), ]
    rownames(result) <- NULL
    return(result)
}

onevariable.summary.with.output <- function(var.x, var.output, train.set, test.set,
                                            digits = 3) {
    #### required libs ####
    source("./R/relative.info.R")
    #### error checking ####
    # >>> todo
    #### function body ####
    if (missing(test.set)) {test.set <- train.set}
    #
    train.values <- train.set[[var.x]]
    train.table  <- table(train.values)
    train.values.freqs <- train.table / nrow(train.set)
    train.values.names <- as.factor(names(train.table))
    #
    test.values  <- test.set[[var.x]]
    test.values.freqs  <- table(test.values) / nrow(test.set)
    test.values.names  <- as.factor(unique(test.values))
    #
    all.values.names   <- union(train.values.names,test.values.names)
    #
    result <- data.frame(value      = all.values.names,
                         freq.train = train.values.freqs[all.values.names],
                         freq.test  = test.values.freqs[all.values.names],
                         PRI.train  = 0)
    result$PRI.train <- sapply(all.values.names,
                               function(v) {
                                   if (v %in% train.values.names) {
                                       relative.info(data = train.set, var.x = var.x,
                                                     var.y = var.output, x.value = v)
                                   } else { NA }
                               })
    result <- result[ order(result$PRI.train, decreasing = TRUE), ]
    rownames(result) <- NULL
    return(result)
}

