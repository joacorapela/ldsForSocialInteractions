computeOneStepAheadObsPredStats <- function(xtt1, Vtt1, Z, a, D, R, obsInputs) {
    dimObs <- nrow(Z)
    nObs <- ncol(xtt1)
    if(!is.na(obsInputs[1])) {
        ytt1 <- Z%*%xtt1+a+D%*%obsInputs
    } else {
        ytt1 <- Z%*%xtt1+a
    }
    Wtt1 <- array(NA, dim=c(dimObs, dimObs, nObs))
    for(n in 1:nObs) {
        Wtt1[,,n] <- Z%*%Vtt1[,,n]%*%t(Z)+R
    }
    answer <- list(ytt1=ytt1, Wtt1=Wtt1)
    return(answer)
}
