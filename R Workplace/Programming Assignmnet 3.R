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

best("TX", "heart attack")
"CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
"FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
"JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
"GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
#"Error in best("BB", "heart attack") : invalid state"
best("NY", "hert attack")
#"Error in best("NY", "hert attack") : invalid outcome"

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

rankhospital("TX", "heart failure", 4)
"DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
"HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
?arrange()

rankall <- function(outcome, num = "best") {
    ######################################## 
    ## Read outcome data
    outcome.csv <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
    ########################################
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
            dplyr::select(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        outcome.filtered[ ,3] <- as.numeric(outcome.filtered[ ,3])
        outcome.filtered <- complete.cases(outcome.filtered[ ,3])
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,3], outcome.filtered[ ,1])
    }
    else if(outcome == "heart failure") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        outcome.filtered <- complete.cases(outcome.filtered[ ,3])
        outcome.filtered <- na.omit(outcome.filtered)
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,3], outcome.filtered[ ,1])
    }
    else {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        outcome.filtered[, 3] <- as.numeric(outcome.filtered[ ,3])
        outcome.filtered <- complete.cases(outcome.filtered[ ,3])
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,3], outcome.filtered[ ,1])
    }
    ######################################## 
    ## rate
    if (tolower(num) == "best") {
        print(dplyr::slice(outcome.sorted, 1))
    }
    else if (tolower(num) == "worst") {
        print(dplyr::slice(outcome.sorted, nrow(outcome.sorted)))
    }
    else {
        print(dplyr::slice(outcome.sorted, 1:as.numeric(num)))
    }
}
head(rankall("heart attack", 20), 10)

rankall <- function(outcome, num = "best") {
    ######################################## 
    ## Read outcome data
    outcome.csv <- read.csv("dataProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
    ########################################
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
            dplyr::select(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        outcome.filtered[ ,3] <- as.numeric(outcome.filtered[ ,3])
        outcome.filtered <- outcome.filtered[complete.cases(outcome.filtered[ ,3]),]
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,3], outcome.filtered[ ,1])
    }
    else if(outcome == "heart failure") {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        outcome.filtered[ ,3] <- as.numeric(outcome.filtered[ ,3])
        outcome.filtered <- complete.cases(outcome.filtered[ ,3])
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,3], outcome.filtered[ ,1])
    }
    else {
        outcome.filtered <- outcome.csv %>%
            dplyr::select(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        outcome.filtered[, 3] <- as.numeric(outcome.filtered[ ,3])
        outcome.filtered <- complete.cases(outcome.filtered[ ,3])
        outcome.sorted <- outcome.filtered %>%
            dplyr::arrange(outcome.filtered[ ,3], outcome.filtered[ ,1])
    }
    ######################################## 
    ## rate
    if (tolower(num) == "best") {
        outcome.grouped <- outcome.sorted %>%
            group_by(State) %>%
            slice(20)
    }
    else if (tolower(num) == "worst") {
        outcome.grouped <- outcome.sorted %>%
            group_by(State) %>%
            slice(-1)
    }
    else {
        outcome.grouped <- outcome.sorted %>%
            group_by(State) %>%
            slice(as.numeric(num)) 
    }
    outcome.grouped %>%
        dplyr::select(Hospital.Name, State)
}



outcome.grouped <- outcome.sorted %>%
    group_by(State) %>%
    filter(row_number() == 1)


vignette("nse")


?slice()






if (tolower(num) == "best") {
    outcome.grouped <- outcome.sorted %>%
        group_by(State) %>%
        summarise(first(Hospital.Name))
}
else if (tolower(num) == "worst") {
    outcome.grouped <- outcome.sorted %>%
        group_by(State) %>%
        summarise(last(Hospital.Name))
}
else {
    outcome.grouped <- outcome.sorted %>%
        group_by(State) %>%
        top_n(as.numeric(num)) 
}
outcome.grouped %>%
    dplyr::select(Hospital.Name, State) 
