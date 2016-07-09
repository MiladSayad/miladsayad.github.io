pathFinder <- function(x, directory = 'specdata') {
    if (x < 10) {
        paste(directory, '/00', x, '.csv', sep = "")
    } else if (x < 100) { 
        paste(directory, '/0', x, '.csv', sep = "")
    } else if (x < 333) {
        paste(directory, '/', x, '.csv', sep = "")
    } else {
        print("Error: The given x is out of range.")
    }
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    data <- NULL
    for (i in id ) {
        path <- pathFinder(x = i, directory = directory)
        new <- read.csv(path, header = TRUE)
        data <- rbind (data, new)
    }
    target.var <- paste('data', '$', pollutant, sep = '')
    mean(eval(parse(text = target.var)), na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
    new <- NULL
    data <- data.frame(x=integer(), y=integer())
    for (i in id ) {
        path <- pathFinder(x = i, directory = directory)
        new <- read.csv(path, header = TRUE)
        new <- new [complete.cases(new),]
        No <- nrow(new)
        data <- rbind(data, c(i, No))
    }
    names(data) = c("id", "nobs")
    data
}

corr <- function(directory , threshold = 0) {
    new <- NULL
    cr <- numeric()
    for (i in 1:332 ) {
        if (complete (directory, i)$nobs >= threshold) {
            path <- pathFinder(x = i, directory = directory)
            new <- read.csv(path, header = TRUE)
            new <- new [complete.cases(new), ]
            new.cor <- cor(new$sulfate, new$nitrate)
            if (!is.na(new.cor)){
                cr <- c(cr, new.cor)
            }
        }
    }
    cr
}


makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

makeVector
