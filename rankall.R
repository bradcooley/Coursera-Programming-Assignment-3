rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the states
        
        ## Check num arg for valid values: best, worst or integer
        if (class(num) == "character") {
                if (! (num == "best" || num == "worst")) {
                        stop ("invalid number")
                }
        } else {num <- round(num)}
        
        ## Read data, keeping only State, Hospital.Name and target outcome column
        if (outcome == "heart attack") {
                mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                                   na.strings="Not Available") [,c(2,7,11)]       
        } else if (outcome == "heart failure") {
                mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                                   na.strings="Not Available") [,c(2,7,17)]     
        } else if (outcome == "pneumonia") {
                mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                                   na.strings="Not Available") [,c(2,7,23)] 
        } else {
                stop("invalid outcome")
        }
        
        ## Assign name and format to column 3, exclude rows with missing values
        names(mydata)[3] = "Death.Rate"
        mydata <- na.omit(mydata)
        mydata[, 3] = suppressWarnings( as.numeric(mydata[, 3]))
        ## Split by State
        mydata.split = split(mydata,mydata$State)
        
        ## use lapply on split data to order the datasplit by outcome, hospital (to break ties)
        ranked <- lapply(mydata.split, function (dat, num) {
                dat = dat[order(dat$Death.Rate, dat$Hospital.Name),]
                ## If best or worst requested, return row 1 or nrow
                if (class(num) == "character") {
                        if (num == "best") {return(dat$Hospital.Name[1])
                        }       else if (num == "worst") {return(dat$Hospital.Name[nrow(dat)])
                        } 
                        ## if specified row greater than number of hospitals for that state, return NA        
                } else if (nrow(dat) < num) {return("NA")
                        ## otherwise, return the ranked hospital       
                } else {return(dat$Hospital.Name[num])
                }
        }, num)
        return (data.frame(hospital = unlist(ranked), state = names(ranked)))
}