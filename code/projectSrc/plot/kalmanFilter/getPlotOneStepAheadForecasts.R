getPlotOneStepAheadForecasts <- function(time, obs, ytt1, Wtt1, boutTimesCol, boutTimesPath, boutTimesFilenames, behaviorsToPlot, behaviorsColors, obsToPlot=NA, xlab="Time (sec)", ylab="Square Root Spike Counts", behaviorsRecsOpacity=0.2) {

    P <- nrow(obs)
    N <- ncol(obs)
    if(is.na(obsToPlot)) {
        obsToPlot <- 1:P
    }

    stdOneStepAheadForecast <- matrix(NA, nrow=P, ncol=N)
    for(i in 1:N) {
        stdOneStepAheadForecast[,i] <- sqrt(diag(Wtt1[,,i]))
    }

    fig <- plot_ly(type='scatter', mode="markers")
    # cols <- brewer.pal(max(3, P), "Set1")
    cols <- colorRampPalette(brewer.pal(9, "Set1"))(max(3, P))
    ymax <- -Inf
    ymin <- +Inf
    for(i in obsToPlot) {
        rgbValues <- col2rgb(cols[i])
        observation <- obs[i,]
        forecastMean <- ytt1[i,]
        cbUpper <- ytt1[i,]+1.96*stdOneStepAheadForecast[i,]
        cbLower <- ytt1[i,]-1.96*stdOneStepAheadForecast[i,]
        ymin <- min(ymin, min(c(observation, cbLower)))
        ymax <- max(ymax, max(c(observation, cbUpper)))
        # observation
        fig <- fig%>%add_trace(x=time, y=obs[i,], mode="markers", name=sprintf("observation[,%d]", i), marker=list(color=sprintf("rgba(%d,%d,%d,%f)", rgbValues[1,1], rgbValues[2,1], rgbValues[3,1], 0.5)))
        # forecast
        rgbaColorName <- sprintf("rgba(%d,%d,%d,%f)", rgbValues[1,1], rgbValues[2,1], rgbValues[3,1], 1)
        fig <- fig%>%add_trace(x=time, y=ytt1[i,], mode="lines+markers", name=sprintf("forecast[,%d]", i), line=list(color=rgbaColorName, dash="solid"), marker=list(color=rgbaColorName, symbol="asterisk-open"))
        fig <- fig%>%add_trace(x=time, y=ytt1[i,]+1.96*stdOneStepAheadForecast[i,], mode="lines", line=list(color="rgba(0,0,0,0)"), name=sprintf("forecast[,%d]", i), showlegend=FALSE)
        fig <- fig%>%add_trace(x=time, y=ytt1[i,]-1.96*stdOneStepAheadForecast[i,], mode="lines", line=list(color="rgba(0,0,0,0)"), name=sprintf("forecast[,%d]", i), showlegend=FALSE, fill="tonexty", fillcolor=sprintf("rgba(%d,%d,%d,%f)", rgbValues[1,1], rgbValues[2,1], rgbValues[3,1], 0.2))
    }
    fig <- fig%>%layout(xaxis=list(title=xlab), yaxis=list(title=ylab))
    behaviorsRecs <- getBehaviorsRecs(boutTimesFilenames=boutTimesFilenames, boutTimesPath=boutTimesPath, behaviorsNames=behaviorsToPlot, behaviorsColors=behaviorsColors, behaviorsRecsOpacity=behaviorsRecsOpacity, ymin=ymin, ymax=ymax)

    fig <- fig%>%layout(shapes=behaviorsRecs)

    return(fig)
}
