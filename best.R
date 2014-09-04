best <- function(state,outcome){
        ## Read outcome data
        bestoc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        num <- data.frame(11,17,23)  #num code 
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        names(num) <- outcomes
        # determine whether the "state" is in the list of state 
        bestoc$State <- as.factor(bestoc$State)
        state_name <- levels(bestoc$State)
        
        if (is.element(state, state_name)){
                if (is.element(outcome,outcomes)){
                        def_state <- split(bestoc,bestoc$State)
                        spec_state <- def_state[[state]]
                        num_state <- as.numeric(spec_state[,num[[outcome]]])
                        num_state <- num_state[!is.na(num_state)]
                        col_min <- min(num_state)
                        hos_num <- which(spec_state[,num[[outcome]]]== col_min)
                        hos_name <- sort(spec_state$Hospital.Name[hos_num])
                        hos_name[1]
                }
                else
                        stop("invalid outcome", call. = TRUE)                
        }
        else
                stop("invalid state", call. = TRUE)
        

        
}
