computePercentageExplainedVar <- function(observations, predictions) {
    dimObs <- nrow(observations)
    pExpVar <- array(rep(NA, dimObs))
    for(i in 1:dimObs) {
        pExpVar[i] <- 1-mean((predictions[i,]-observations[i,])^2)/var(observations[i,])
    }
    return(pExpVar)
}
