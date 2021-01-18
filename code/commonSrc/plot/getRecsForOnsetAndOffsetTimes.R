
getRecsForOnsetAndOffsetTimes <- function(onsetTimes, offsetTimes, color, opacity, ymin, ymax) {
    recs <- vector(mode="list", length=length(onsetTimes))
    for(i in 1:length(onsetTimes)) {
        x0 <- onsetTimes[i]
        x1 <- offsetTimes[i]
        recs[[i]] <- list(type="rect",
                                fillcolor=color, 
                                line=list(color=color), 
                                opacity=opacity,
                                x0=x0, x1=x1, xref="x",
                                y0=ymin, y1=ymax, yref="y")
    }
    return(recs)
}
