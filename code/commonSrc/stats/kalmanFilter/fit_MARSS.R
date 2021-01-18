fit_MARSS <- function(observations, inits, stateDim, stateInputs, stateOffsetType, stateCovType, obsInputs, obsOffsetType, obsCovType, initialStateMeanType, initialStateCovType, maxIter, kfFunc, silentLevel=2) {
    MLEobj <- create_MARSS(observations=observations,
                           inits=inits,
                           stateDim=stateDim,
                           stateInputs=stateInputs,
                           stateOffsetType=stateOffsetType,
                           stateCovType=stateCovType,
                           obsInputs=obsInputs,
                           obsOffsetType=obsOffsetType,
                           obsCovType=obsCovType,
                           initialStateMeanType=initialStateMeanType,
                           initialStateCovType=initialStateCovType,
                           maxIter=maxIter,
                           kfFunc=kfFunc,
                           silentLevel=silentLevel)
    kem <- MARSSkem(MLEobj=MLEobj)
    return(kem)
}
