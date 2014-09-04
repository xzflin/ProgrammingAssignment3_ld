rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        bestoc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        numb <- data.frame(11,17,23)  #num code 
        
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        names(numb) <- outcomes
        
        bestoc$State <- as.factor(bestoc$State)
        
        state_name <- levels(bestoc$State)
        if (is.element(state, state_name)){
                if (is.element(outcome,outcomes)){
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
                        def_state <- split(bestoc,bestoc$State)
                        spec_state <- def_state[[state]]
                        spec_state[,numb[[outcome]]] <- as.numeric(spec_state[,numb[[outcome]]])
                        kp <- spec_state[,c(2,numb[[outcome]])]
                        good <- complete.cases(kp)
                        kp_good <- kp[good,]
                        num_state <- kp_good[order(kp_good[,2],kp_good[,1]),]
                        
                        if (num == "worst"){
                                num_state$Hospital.Name[dim(num_state)[1]]
                        }
                        else if (num == "best"){
                                num_state$Hospital.Name[1]
                        }
                        else if(num <= dim(num_state)[1]){
                                num_state$Hospital.Name[num]
                        }
                        else
                                NA
                }           
                else
                                stop("invalid outcome", call. = TRUE) 
        }
        else
                stop("invalid state", call. = TRUE)
        
        
        
}
