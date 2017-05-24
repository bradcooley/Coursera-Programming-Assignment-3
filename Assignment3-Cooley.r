## Programming Assignment 3 - R Programming Course
## Brad Cooley  May 20, 2017

setwd("~/Desktop/Coursera/Prog Assignment  week3")



best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ## Read the outcome dataset - all as character
        mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                           na.strings="Not Available")
        
        found.state <- which(unique(mydata$State) == state)
        if(length(found.state) < 1 ) {stop("invalid state")}
        
        valid.outcomes <-  c("heart attack","heart failure","pneumonia")
        if (! outcome %in% valid.outcomes) {stop("invalid outcome")}
        
        ## get correct column name for mortality rates
        mortality.cols <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        col <- match(outcome,valid.outcomes)
        
        ## get the subset of rows for requested state
        state.data <- mydata[mydata$State==state,]
        
        ## find the row with min mortality rate
        minhosp <- which.min(as.double(state.data[,mortality.cols[col]]))
        
        return(  state.data[minhosp,"Hospital.Name"])
}
