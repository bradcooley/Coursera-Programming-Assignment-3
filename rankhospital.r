rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
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
        # Omit NA values for outcome of interest
        state.data <- state.data[!is.na(state.data[,mortality.cols[col]]),]
        # Convert outcome column to numeric and sort by outcome col, city
        state.data <- state.data[order(as.numeric(state.data[,mortality.cols[col]]),state.data[,"Hospital.Name"]),]
        num.hospitals <- nrow(state.data)
        
        ## Find the target rank within the state for the outcome (allows best and worst)
        ## minhosp <- which.min(as.double(state.data[,mortality.cols[col]]))
        
        if  (num == "best") {target.hosp <- state.data[1,"Hospital.Name"]  }
                else if (num == "worst") {target.hosp <- state.data[nrow(state.data),"Hospital.Name"]}
                        else if (as.integer(num) > nrow(state.data)) {target.hosp <- "NA"}
                                else {target.hosp <- state.data[as.integer(num),"Hospital.Name"]}
        return(target.hosp)
}