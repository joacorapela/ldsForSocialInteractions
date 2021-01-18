
getStimRecs <- function(goStimOn, goStimOff, nogoStimOn, nogoStimOff, laserStimOn, laserStimOff, ymin, ymax, goStimColor="green", nogoStimColor="red", laserStimColor="blue", stimOpacity=0.2) {

    getRecs <- function(onsets, offsets, color, opacity, ymin, ymax) {
        recs <- vector(mode="list", length=length(onsets))
        for(i in 1:length(onsets)) {
            x0 <- onsets[i]
            x1 <- offsets[i]
            recs[[i]] <- list(type="rect",
                              fillcolor=color,
                              line=list(color=color),
                              opacity=opacity,
                              x0=x0, x1=x1, xref="x",
                              y0=ymin, y1=ymax, yref="y")
        }
        return(recs)
    }

    goStimRecs <- getRecs(onsets=goStimOn, offsets=goStimOff, color=goStimColor, opacity=stimOpacity, ymin=ymin, ymax=ymax)
    nogoStimRecs <- getRecs(onsets=nogoStimOn, offsets=nogoStimOff, color=nogoStimColor, opacity=stimOpacity, ymin=ymin, ymax=ymax)
    laserStimRecs <- getRecs(onsets=laserStimOn, offsets=laserStimOff, color=laserStimColor, opacity=stimOpacity, ymin=ymin, ymax=ymax)

    stimRecs <- c(goStimRecs, nogoStimRecs, laserStimRecs)

    return(stimRecs)
}

