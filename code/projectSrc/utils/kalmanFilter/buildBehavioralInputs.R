
buildBehavioralInputs <- function(behavioralTimeSeries, inputMemory) {
    buildInputsBlock <- function(stim, inputMemory) {
        inputs <- c()
        N <- length(stim)
        for(i in 0:(inputMemory)) {
            inputs <- rbind(inputs, c(rep(0, times=i), stim[1:(N-i)]))
        }
        return(inputs)
    }

    allInputs <- c()
    for(i in 1:nrow(behavioralTimeSeries)) {
        inputsBlock <- buildInputsBlock(stim=behavioralTimeSeries[i,], inputMemory=inputMemory)
        allInputs <- rbind(allInputs, inputsBlock)
    }
    return(allInputs)
}

