library(reticulate)
np <- import("numpy")

getBehaviorsRecs <- function(boutTimesFilenames, boutTimesPath, behaviorsNames, behaviorsColors, behaviorsRecsOpacity, ymin, ymax) {
    behaviorsRecs <- list()
    for(boutTimesFilename in boutTimesFilenames) {
        boutTimesFullFilename <- file.path(boutTimesPath, boutTimesFilename)
        boutTimes <- np$load(boutTimesFullFilename)
        for(i in 1:length(behaviorsNames)) {
            behaviorName <- behaviorsNames[i]
            behaviorColor <- behaviorsColors[i]
            behaviorBoutTimes <- boutTimes[[behaviorName]]
            behaviorsRecs <- append(behaviorsRecs, getRecsForOnsetAndOffsetTimes(onsetTimes=behaviorBoutTimes[,1], offsetTimes=behaviorBoutTimes[,2], color=behaviorColor, opacity=behaviorsRecsOpacity, ymin=ymin, ymax=ymax))
        }
    }
    return(behaviorsRecs)
}
