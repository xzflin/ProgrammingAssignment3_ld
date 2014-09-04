rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        bestoc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        numb <- data.frame(11,17,23)  #num code 

        
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        names(numb) <- outcomes
        
                if (is.element(outcome,outcomes)){

                        

                        bestoc[,numb[[outcome]]] <- as.numeric(bestoc[,numb[[outcome]]])
                        kp <- bestoc[,numb[[outcome]]]
                        good <- complete.cases(kp)
                        kp_good <- bestoc[good,]
                        def_state <- split(kp_good,kp_good$State)
                        num_state <- lapply(def_state, function(x) x[order(x[,numb[[outcome]]],x$Hospital.Name),])
                       # row_num <- sapply(num_state, function(x) dim(x)[1])
                        
                        if (num == "worst"){
                                result <- sapply(num_state, function(x) x$Hospital.Name[dim(x)[1]])

                                
                        }
                        else if (num == "best"){
                                result <- sapply(num_state, function(x) x$Hospital.Name[1])     
                        }
                        
                        else 
                                result <- sapply(num_state, function(x) x$Hospital.Name[num])
                        result <- data.frame(result)
                        names(result) <- c("hospital")
                       result$state <- row.names(result)
 #                       row.names(result) <- row.names(data.frame(row_num))
                        
                        result
                }           
                else
                        stop("invalid outcome", call. = TRUE) 
                
}
