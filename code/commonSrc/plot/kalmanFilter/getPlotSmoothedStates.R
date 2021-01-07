getPlotSmoothedStates <- function(time, xtT, VtT, goStimOn, goStimOff, nogoStimOn, nogoStimOff, laserStimOn, laserStimOff, statesToPlot=NA, xlab="Time (sec)", ylab="Smoothed State", goStimColor="green", nogoStimColor="red", laserStimColor="blue", stimOpacity=0.2) {

    M <- nrow(xtT)
    N <- ncol(xtT)
    if(is.na(statesToPlot[1])) {
        statesToPlot <- 1:M
    }

    stds <- matrix(NA, nrow=M, ncol=N)
    for(i in 1:N) {
        stds[,i] <- sqrt(diag(VtT[,,i]))
    }

    fig <- plot_ly(type='scatter', mode="markers")
    # cols <- brewer.pal(max(3, M+nInputs), "Set1")
    cols <- colorRampPalette(brewer.pal(9, "Set1"))(max(3, M))
    ymax <- -Inf
    ymin <- +Inf
    for(i in statesToPlot) {
        rgbValues <- col2rgb(cols[i])
        mean <- xtT[i,]
        std <- stds[i,]
        cbUpper <- mean+1.96*std
        cbLower <- mean-1.96*std
        ymax <- max(ymax, max(cbUpper))
        ymin <- min(ymin, min(cbLower))
        rgbaColorName <- sprintf("rgba(%d,%d,%d,%f)", rgbValues[1,1], rgbValues[2,1], rgbValues[3,1], 1)
        fig <- fig%>%add_trace(x=time, y=mean, mode="lines+markers", name=sprintf("state%d", i), line=list(color=rgbaColorName, dash="solid"), marker=list(color=rgbaColorName, symbol="asterisk-open"))
        fig <- fig%>%add_trace(x=time, y=cbUpper, mode="lines", line=list(color="rgba(0,0,0,0)"), name=sprintf("state[,%d]", i), showlegend=FALSE)
        fig <- fig%>%add_trace(x=time, y=cbLower, mode="lines", line=list(color="rgba(0,0,0,0)"), name=sprintf("state[,%d]", i), showlegend=FALSE, fill="tonexty", fillcolor=sprintf("rgba(%d,%d,%d,%f)", rgbValues[1,1], rgbValues[2,1], rgbValues[3,1], 0.2))
    }
    fig <- fig%>%layout(xaxis=list(title=xlab), yaxis=list(title=ylab))
    stimRecs <- getStimRecs(goStimOn=goStimOn, goStimOff=goStimOff, nogoStimOn=nogoStimOn, nogoStimOff=nogoStimOff, laserStimOn=laserStimOn, laserStimOff=laserStimOff, ymin=ymin, ymax=ymax, goStimColor=goStimColor, nogoStimColor=nogoStimColor, laserStimColor=laserStimColor, stimOpacity=stimOpacity)
    fig <- fig%>%layout(shapes=stimRecs)

    return(fig)
}
