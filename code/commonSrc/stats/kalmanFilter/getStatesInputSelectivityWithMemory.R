getStatesInputSelectivityWithMemory <- function(C, B, memorySamples) {
    nStates <- nrow(C)
    nStim <- ncol(C)
    S <- array(NA, dim=c(nStates, nStim, memorySamples+1))
    Bpow <- diag(rep(1, times=nStates))
    for(i in 0:memorySamples) {
        S[,,i] <- Bpow%*%C
    }
    return(S)
}

