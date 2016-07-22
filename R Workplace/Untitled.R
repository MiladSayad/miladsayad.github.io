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
