install.packages("dplyr")
library("dplyr")
install.packages("tidyr")
library("tidyr")


outcome <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


outcome1 <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", header = TRUE)
str(outcome1)

best <- function(state, outcome) {
    ######## Read outcome data
    outcome.csv <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)

    ######## Check that state and outcome are valid
    # State validity
    if(!(state %in% outcome.csv$State)) stop("The state does not exist!!!")
    # Outcome validity
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("The outcome does not exist!!!")
    
    ######## Return hospital name in that state with lowest 30-day death
    if(outcome == "heart attack") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                          Hospital.Name) %>%
            dplyr::filter(outcome.csv$State == state)
        outcome.filtered[, 2] <- as.numeric(outcome.filtered[ ,2])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,2], outcome.filtered[ ,3])
    }
    else if(outcome == "heart failure") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                          Hospital.Name) %>%
            dplyr::filter(outcome.csv$State == state)
        outcome.filtered[, 2] <- as.numeric(outcome.filtered[ ,2])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,2], outcome.filtered[ ,3])
    }
    else {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) %>%
            dplyr::filter(outcome.csv$State == state)
        outcome.filtered[, 2] <- as.numeric(outcome.filtered[ ,2])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,2], outcome.filtered[ ,3])
    }
    ## rate
    print(outcome.sorted[1,3])
}

rankhospital <- function(state, outcome, num = "best") {
    
    ######################################## 
    ## Read outcome data
    outcome.csv <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
    ######################################## 
    ##Check that state and outcome are valid
    # State validity check
    if(!(state %in% outcome.csv$State)) stop("The state does not exist!!!")
    # Outcome validity check
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("The outcome does not exist!!!")
    # Num validity check
    if (is.na(as.numeric(num))) {
        if (tolower (num) != "best" & tolower (num) != "worst") stop("The num is incorrect!!!")
    }
    
    ######################################## 
    ## Return hospital name in that state with the lowest 30-day death
    if(outcome == "heart attack") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name) %>%
            dplyr::filter(outcome.csv$State == state)
        outcome.filtered[ ,2] <- as.numeric(outcome.filtered[ ,2])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,2], outcome.filtered[ ,3])
    }
    else if(outcome == "heart failure") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name) %>%
            dplyr::filter(outcome.csv$State == state)
        outcome.filtered[ ,2] <- as.numeric(outcome.filtered[ ,2])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,2], outcome.filtered[ ,3])
    }
    else {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) %>%
            dplyr::filter(outcome.csv$State == state)
        outcome.filtered[, 2] <- as.numeric(outcome.filtered[ ,2])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,2], outcome.filtered[ ,3])
    }
    ######################################## 
    ## rate
    if (tolower(num) == "best") {
        print(dplyr::first(outcome.sorted[ ,3]))
    }
    else if (tolower(num) == "worst") {
        print(dplyr::last(outcome.sorted[ ,3]))
    }
    else {
        print(dplyr::nth(outcome.sorted[,3], as.numeric(num)))
    }
}

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome.csv <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
    ## Outcome validity check
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("The outcome does not exist!!!")
    # Num validity check
    if (is.na(as.numeric(num))) {
        if (tolower (num) != "best" & tolower (num) != "worst") stop("The num is incorrect!!!")
    }
    ######################################## 
    ## Return hospital name in that state with the lowest 30-day death
    if(outcome == "heart attack") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Heart.Attack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    }
    else if(outcome == "heart failure") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Heart.Failure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    }
    else {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    }
    outcome.filtered[ , 3] <- as.numeric(outcome.filtered[ , 3])
    outcome.filtered$State <- as.factor(outcome.filtered$State)
    if (tolower (num) == "worst") outcome.filtered <- na.omit(outcome.filtered)
    outcome.split <- split(outcome.filtered, outcome.filtered$State, drop = FALSE)
    outcome.sorted <- lapply (outcome.split, function(x) arrange(x, x[ ,3], x[ ,1]))
    
    if (tolower (num) == "best") outcome.selected <- sapply (outcome.sorted, function(x) slice (x, 1))
    else if (tolower (num) == "worst") outcome.selected <- sapply (outcome.sorted, function(x) filter (x, row_number()== n()))
    else outcome.selected <- sapply (outcome.sorted, function(x) slice (x, num))
    
    outcome.selected <- as.data.frame(outcome.selected)
    outcome.selected [2, ] <- names(outcome.selected)
    outcome.transposed <- as.data.frame (t(outcome.selected))
    for(i in 1:nrow(outcome.transposed)) {
        if (outcome.transposed[i,1] == "character(0)") {
            outcome.transposed[i,1] <- "<NA>"
            print(i)
        }
            print(num)
    }
    outcome.transposed
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("HI", "heart attack", 4)

tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, State == "HI")$Hospital.Name)

head(rankall("heart attack", 20), 10)

r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")$Hospital.Name)
