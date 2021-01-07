
getBehaviorsOnsetsAndOffsets <- function(behaviorsToUse, boutTimesFilenames) {
    behaviorsOnsetsAndOffsets <- list()
    for(i in 1:length(behaviorsToUse)) {
        behaviorsOnsetsAndOffsets[[i]] <- c()
    }
    for(boutTimesFilename in boutTimesFilenames) {
        boutTimesFullFilename <- file.path(boutTimesPath, boutTimesFilename)
        boutTimes <- np$load(boutTimesFullFilename)
        for(i in 1:length(behaviorsToUse)) {
            behaviorToUse <- behaviorsToUse[i]
            behaviorBoutTimes <- boutTimes[[behaviorToUse]]
            behaviorsOnsetsAndOffsets[[i]] <- rbind(behaviorsOnsetsAndOffsets[[i]], behaviorBoutTimes)
        }
    }
    names(behaviorsOnsetsAndOffsets) <- behaviorsToUse
    return(behaviorsOnsetsAndOffsets)
}
